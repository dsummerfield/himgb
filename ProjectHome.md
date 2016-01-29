### A small imageboard written in Haskell ###

good:
  * has threads, image/text replies
  * threads are bumped when replying
  * rudimentary sanity checks
  * makes nice thumbs
  * no calls to outside binaries or any evals anywhere
  * `*` small - less than 200 lines of readable code
  * `*` fast - compiled to machine code
  * `*` automatically terminates if not finished in a reasonable amount of time

`*` - the only image board on the market with this feature!

to do:
  * ~~have it accept files other than jpegs~~ fixed
  * ~~make threads expire~~ fixed
  * make it more configurable
  * use a hashing algorithm to reject duplicates
  * fix the formatting
  * implement pages
  * implement some sort of admin panel
  * use some sort of database instead of text files
  * make it run on non-UNIX systems by rewriting System.POSIX.Time.epochTime

source:
  * http://himgb.googlecode.com/svn/trunk/hImgb.hs

prerequisites:
  * UNIX
  * GHC >= 6.6
  * HTTP server with CGI support
  * Graphics.GD

installing:
  * `ghc --make hImgB.hs -o index.cgi`
  * `mkdir /src`
  * `./index.cgi purge`