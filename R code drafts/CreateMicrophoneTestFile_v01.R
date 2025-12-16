# Translated in R from Matt Ayres R code 
#July 2025

# Load necessary libraries
library(tuneR)
library(audio)

# Set working directory
root_directory <- getwd()

# Sampling frequency for the sound file
Fs <- 44100                            # Sampling frequency
secs <- 0.5                            # Duration of tones
pause <- 0.1                           # Pause between tones
tpause <- seq(0, pause, length.out = Fs * pause)  # Time pause vector

fmax <- 10
freqstep <- 12000 / fmax               # Create vector of frequencies
freq <- seq(freqstep, by = freqstep, length.out = fmax)  # Frequency vector

amax <- 5
ampstep <- 1 / amax                    # Create vector of amplitudes
amp <- seq(ampstep, by = ampstep, length.out = amax)  # Amplitude vector

soundfull <- numeric(0)                # Initialize the sound full vector

for (f in 1:fmax) {                    # For each frequency
  for (a in 1:amax) {                  # For each amplitude
    t <- seq(0, secs, length.out = Fs * secs + 1)  # Time vector + 1 sample
    t <- t[-length(t)]                 # Remove extra sample
    w <- 2 * pi * freq[f]              # Radian value to create tone
    s <- sin(w * t)                    # Create tone
    s <- s * amp[a]                    # Adjust amplitude
    
    s <- c(tpause, s)                  # Add pause
    soundfull <- c(soundfull, s)       # Combine new tone with the growing vector
  }
}

# Normalize soundfull to be within range
soundfull <- soundfull / max(abs(soundfull))

# Write to a wave file
sound_full_wave <- Wave(soundfull, samp.rate = Fs, bit = 16)
writeWave(sound_full_wave, file.path("./audio_files/MicrophoneTestFile.v02.wav"))

# Play the sound directly in R
play(sound_full_wave)

