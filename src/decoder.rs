use opus::{Channels, Decoder};
use rsmpeg::ffi::{AV_SAMPLE_FMT_FLT, AV_SAMPLE_FMT_S16};
use steam_audio_codec::{Packet, SteamVoiceData};
use thiserror::Error;

use crate::SAMPLE_RATE;

const FRAME_SIZE: usize = 960;

#[derive(Debug, Error)]
pub enum DecoderError {
    #[error("Insufficient data")]
    InsufficientData,
    #[error("Opus Error: {0}")]
    OpusError(#[from] opus::Error),
}

pub struct SteamVoiceDecoder {
    decoder: Decoder,
    seq: u16,
    decoder_kind: SampleDecoderKind,
}

fn read_bytes<const N: usize>(data: &[u8]) -> Result<([u8; N], &[u8]), DecoderError> {
    let Some((result, rest)) = data.split_at_checked(N) else {
        return Err(DecoderError::InsufficientData);
    };
    Ok((result.try_into().unwrap(), rest))
}

fn read_u16(data: &[u8]) -> Result<(u16, &[u8]), DecoderError> {
    let (bytes, data) = read_bytes(data)?;
    Ok((u16::from_le_bytes(bytes), data))
}

#[derive(Debug)]
enum SampleDecoderKind {
    Float,
    S16,
}

impl SampleDecoderKind {
    pub fn decode(
        &self,
        decoder: &mut Decoder,
        input: &[u8],
        output: &mut [u8],
    ) -> Result<usize, DecoderError> {
        match self {
            SampleDecoderKind::S16 => {
                let output_length = if input.is_empty() {
                    FRAME_SIZE
                } else {
                    output.len()
                } / std::mem::size_of::<i16>();
                let out = unsafe {
                    std::slice::from_raw_parts_mut(output.as_mut_ptr() as *mut i16, output_length)
                };
                let n = decoder.decode(input, out, false)?;
                Ok(n * std::mem::size_of::<i16>())
            }
            SampleDecoderKind::Float => {
                let output_length = if input.is_empty() {
                    FRAME_SIZE
                } else {
                    output.len()
                } / std::mem::size_of::<f32>();
                let out = unsafe {
                    std::slice::from_raw_parts_mut(output.as_mut_ptr() as *mut f32, output_length)
                };
                let n = decoder.decode_float(input, out, false)?;
                Ok(n * std::mem::size_of::<f32>())
            }
        }
    }
}

impl SteamVoiceDecoder {
    pub fn new(sample_format: i32) -> Result<Self, DecoderError> {
        let decoder = Decoder::new(SAMPLE_RATE as u32, Channels::Mono)?;
        let decoder_kind = match sample_format {
            AV_SAMPLE_FMT_S16 => SampleDecoderKind::S16,
            AV_SAMPLE_FMT_FLT => SampleDecoderKind::Float,
            _ => panic!("decoder created with sample format that we didn't account for!"),
        };

        Ok(Self {
            decoder,
            seq: 0,
            decoder_kind,
        })
    }

    pub fn decode(
        &mut self,
        voice_data: SteamVoiceData,
        output_buffer: &mut [u8],
    ) -> Result<usize, Box<dyn std::error::Error>> {
        let mut total = 0;
        for packet in voice_data.packets() {
            let packet = packet.expect("Coudln't read packet??");
            match packet {
                Packet::SampleRate(rate) => {
                    if rate != SAMPLE_RATE as u16 {
                        panic!("Sample rate was something other than {SAMPLE_RATE}!");
                    }
                }
                Packet::OpusPlc(opus) => {
                    let size = self.decode_opus(opus.as_slice(), &mut output_buffer[total..])?;
                    total += size;
                    if total >= output_buffer.len() {
                        return Err("InsufficientOutputBuffer".into());
                    }
                }
                Packet::Silence(silence) => {
                    total += silence as usize * std::mem::size_of::<i16>();
                }
            }
        }
        Ok(total)
    }

    fn decode_opus(
        &mut self,
        mut data: &[u8],
        output_buffer: &mut [u8],
    ) -> Result<usize, DecoderError> {
        let mut total = 0;
        while data.len() > 2 {
            let (len, remainder) = read_u16(data)?;
            data = remainder;
            if len == u16::MAX {
                self.decoder.reset_state()?;
                self.seq = 0;
                continue;
            }
            let (seq, remainder) = read_u16(data)?;
            data = remainder;

            if seq < self.seq {
                self.decoder.reset_state()?;
            } else {
                let lost = (seq - self.seq).min(10);
                for _ in 0..lost {
                    let count = self.decoder_kind.decode(
                        &mut self.decoder,
                        &[],
                        &mut output_buffer[total..],
                    )?;
                    total += count;
                    if total >= output_buffer.len() {
                        return Err(DecoderError::InsufficientData);
                    }
                }
            }
            let len = len as usize;

            self.seq = seq + 1;

            if data.len() < len {
                return Err(DecoderError::InsufficientData);
            }

            let count = self.decoder_kind.decode(
                &mut self.decoder,
                &data[0..len],
                &mut output_buffer[total..],
            )?;
            data = &data[len..];
            total += count;
            if total >= output_buffer.len() {
                return Err(DecoderError::InsufficientData);
            }
        }

        Ok(total)
    }
}
