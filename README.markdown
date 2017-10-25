My entry in the Lisp Game Jam (Hard Mode) for October 2017.

License: MIT/X11

Music by Rolemusic: <http://freemusicarchive.org/music/Rolemusic/The_Black_Dot/>
Sound by timgormly: <https://freesound.org/people/timgormly/packs/10094/>

Building Locally
================

You might be able to build this on OS X and *maybe* Linux.  Maybe.  Sorry, Lisp
deployment is still a shitshow in 2017.

Clone this repo and two others into your Quicklisp `local-projects` directory:

    git clone https://github.com/sjl/batty.git
    git clone https://github.com/sjl/cl-losh.git
    git clone https://github.com/sjl/cl-blt.git

Change to the `batty` directory.  You have to run from there, sorry:

    cd batty

Then open SBCL and:

    (ql:quickload :batty)
    (b:run)

If you get a division by zero error, make sure you're not using USB speakers.
Yeah, I know.  Computers are garbage.



