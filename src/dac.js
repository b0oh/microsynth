export default function (app) {
  const audioContext =
    new (window.AudioContext || window.webkitAudioContext)()

  const sampleRate = 44100
  const samplesPerSec = sampleRate / 1000

  let startTime = 0
  let sampleNumber = 0
  let running = false

  function start() {
    if (audioContext.state === 'suspended') {
        audioContext.resume()
    }

    startTime = audioContext.currentTime
    sampleNumber = 0
    running = true

    requestSamples(sampleNumber)
  }

  function stop() {
    running = false
  }

  function requestSamples(sampleNumber) {
    if (running) {
      app.ports.dacRequestSamples.send(sampleNumber)
    }
  }

  function queueSamples(samples) {
    const buffer =
      audioContext.createBuffer(1, samples.length, sampleRate)

    const channel =
      buffer.getChannelData(0)

    for (let ix = 0; ix < buffer.length; ix++) {
      channel[ix] = samples[ix]
    }

    const source = audioContext.createBufferSource()

    source.buffer = buffer
    source.connect(audioContext.destination)

    const when = startTime + (sampleNumber / sampleRate)
    const delay = (when - audioContext.currentTime) * 1000

    source.start(when)
    window.setTimeout(() => requestSamples(sampleNumber), delay)

    sampleNumber = sampleNumber + samples.length
  }

  app.ports.dacQueueSamples.subscribe(queueSamples)
  app.ports.dacStart.subscribe(start)
  app.ports.dacStop.subscribe(stop)
}
