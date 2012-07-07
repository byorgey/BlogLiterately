I have now released version 0.5 of [`BlogLiterately`][]. (You can
[read about the 0.4 release
here](http://byorgey.wordpress.com/2012/07/02/blogliterately-0-4-release/).)
This version does uploading of images! Here is proof:

 <div style='text-align:center;'>
![](../test/images/puppy-small.jpg)
 </div>

d'awww.

([My previous
post](http://byorgey.wordpress.com/2012/07/07/new-haxr-release/)
explains what the problem and solution was with image uploads.)

It also allows you to specify *expected outputs* in a `ghci` session
(a feature [suggested by Dan
Burton](http://www.reddit.com/r/haskell/comments/vz5vn/blogliterately_04_brent_yorgeys_tool_for/c5936th)).  This block

    [other]
        [ghci]
        7+6
          13
        9+4
          12

now produces

    [ghci]
    7+6
      13
    9+4
      12

Outputs that match the expected output are shown normally; outputs
that don't match the expected output are shown with the actual output
in red and expected in blue.  The idea is that this helps you catch
errors in your code before uploading the post. (Of course, you don't
have to specify expected outputs if you don't want to.)

Another new feature is that `BlogLiterately` will prompt you for your password if
you don't specify it on the command line (another feature requested by
Dan).

Finally, one of the coolest new features (in my opinion) is that the
internals are now [exposed as a
library](http://hackage.haskell.org/package/BlogLiterately), and in
particular you can [easily add your own custom transformation passes](XXX link to Run)
(of type `Pandoc -> IO Pandoc`) to the existing ones.  So, for
example, you could do something particular with your own specially
tagged blocks (like `[ghci]` blocks), or wrap images in some fancy
HTML to produce frames and captions, or automatically turn certain
things into links, or whatever you can dream up.  If you come up with
any transformations you think might be more generally useful, please
send them to me so I can include them in future releases for others to
use.

Happy blogging!

[`BlogLiterately`]: http://hackage.haskell.org/package/BlogLiterately