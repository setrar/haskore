.PHONY:	ghci jack jack-rt supercollider supercollider-rt scj

ghci:
	ghci -i:src src/Haskore/Interface/SuperCollider/Example.hs

jack:
# -n 2 is default, larger values are not supported
	jackd --nozombies -d alsa -Xseq -r 44100 -n 3 -d hw:0 &
#	jackd -d alsa -Xseq -r 44100 -d hw:0 &

jack-stop:
	pkill jackd

supercollider:
	(export SC_JACK_DEFAULT_OUTPUTS=system; \
	 scsynth -u 57110 -D 0)

supercollider-stop:
	kill `pgrep scsynth`

scj:	jack supercollider


jack-rt:
# block size 4096 freezes the machine
# more than two buffer blocks (-n 2) are not supported
#	jackd --realtime -d alsa -r 44100 -p 4096 -d hw:0 &
	jackd --realtime -d alsa -Xseq -r 44100 -d hw:0 &

scj-rt:	jack-rt supercollider
	echo "start with 'sudo make scj-rt'"


jack-sudo:
# causes repeated message "delay of 68264.000 usecs exceeds estimated spare time of 23188.000; restart ..."
#	sudo jackd -R -d alsa -r 44100 -n 2 -d hw:0 &
#	sudo jackd -P 15 -d alsa -r 44100 -n 2 -d hw:0 &
	sudo jackd --realtime -d alsa -r 44100 -n 2 -d hw:0 &

supercollider-sudo:
	echo 'export SC_JACK_DEFAULT_OUTPUTS=alsa_pcm:playback_1,alsa_pcm:playback_2; scsynth -u 57110 -D 0' | sudo bash

scj-sudo:	jack-sudo supercollider-sudo


# targets for adding a video to the audio
avi:
	(for id in `seq 1 184`; do echo /data1/daniel/fractal/animation.avi; done) > loop-list
	mencoder `cat loop-list` -o loop.avi -ovc copy
	mencoder loop.avi -o air.avi -ovc lavc -audiofile $(HOME)/programming/haskell/synthesizer/hal2/supercollider-haskore/Air.mp3 -oac copy
# should work, but the generated AVI cannot be played with MPlayer
#	avimerge -o air.avi -i /data1/daniel/fractal/animation.avi /data1/daniel/fractal/animation.avi /data1/daniel/fractal/animation.avi -p hal2/supercollider-haskore/Air.mp3

record:
	recordmydesktop -y 10 \
	   --windowid `xwininfo | grep 'Window id:' | cut -f4 -d' '`
#	   --device hw:0,0
#	   --use-jack SuperCollider:out_1 SuperCollider:out_2

air:
	./dist/build/air/air

animation:
	animate /data1/daniel/fractal/animation.avi
