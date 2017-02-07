# Video file decoding in Common Lisp

Simple AVI/MJPEG video decoder written in Common Lisp. Leverages [CL-JPEG](https://github.com/sharplispers/cl-jpeg) for frame processing and [CL-RIFF](https://github.com/RobBlackwell/cl-riff) for container format handling.

A primitive CLX media player is included.

Has only been lightly tested on SBCL 13.x/Linux x86-64.

Some sample files can be found [here](https://cinelerra-cv.org/footage.php) (the toy plane AVI) and [here](http://jjc.freeshell.org/turning_pages.html).


## Known Limitations

* No indexing support
* No aduio (however the stream is properly handled)


## TODO:

* AVI MJPEG chunk decoding [done]
* Rudimentary video stream player [done]
* Indexing suppot
