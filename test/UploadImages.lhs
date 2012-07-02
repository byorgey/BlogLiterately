Half-baked code here, for use if/when image uploading gets worked out.  See
http://stackoverflow.com/questions/11277788/errorclosed-exception-from-network-http-simplehttp-trying-to-upload-images-vi

> uploadAllImages :: BlogLiterately -> (Pandoc -> IO Pandoc)
> uploadAllImages bl@(BlogLiterately{..}) =
>   case (blog, uploadImages) of
>     (Just xmlrpc, True) -> bottomUpM (uploadOneImage xmlrpc)
>     _                   -> return
>   where
>     uploadOneImage :: String -> Inline -> IO Inline
>     uploadOneImage xmlrpc i@(Image altText (imgUrl, imgTitle))
>       | isLocal imgUrl = do
>           res <- uploadIt imgUrl bl
>           case lookup "url" res of
>             Just newUrl -> return $ Image altText (newUrl, imgTitle)
>             Nothing     -> do
>               putStrLn $ "Warning: upload of " ++ imgUrl ++ " failed."
>               return i
>       | otherwise      = return i
>     uploadOneImage _ i = return i
>
>     isLocal imgUrl = none (`isPrefixOf` imgUrl) ["http", "/"]
>     none p = all (not . p)
>
> uploadIt :: FilePath -> BlogLiterately -> IO [(String, String)]
> uploadIt file (BlogLiterately{..}) = do
>   media <- mkMediaObject file
>   remote url "metaWeblog.newMediaObject" blogid user password media


Half-written text for manual:

When passed the `--upload-images` option, `BlogLiterately` can take
any images referenced locally and automatically upload them to the
server, replacing the local references with appropriate URLs.

To include images in blog posts, use the Markdown syntax

    ![alt](URL)   XXX is there more to it than this?  What about title?

The URL determines whether the image will be uploaded. A *remote* URL
is any beginning with `http` or a forward slash.  In all other cases
it is assumed that the URL in fact represents a relative path on the
local file system.  Such images, if they exist, will be uploaded to
the server (using the `metaWeblog.newMedia` (XXX) RPC call), and the
local file name replaced with the URL returned by the server.

XXX how should this be done?  What key does "replace" use?
Note that the "replace" option is passed to `metaWeblog.newMedia`
(XXX), so 

XXX finish me
