# text-editor
You can build the project with [stack](https://www.haskellstack.org) via `stack install`
It can be run like `cat test/test.txt | text-editor-exe` or `text-editor-exe -f test/test.txt`


Some design decisions:


* IO input and processing are not streaming. I consider this OK since the input format explicitly includes a length header and it simplifies the design greatly. It would be nice though to have an interactive format like ED that will accept commands as they arrive, in which case proper streaming would be essential.
* I choose to use `WriterT` for the output for easier testing and repurposing, as it is not tied to `stdout`. The tradeoff is that output is not streamed, and may build up O(n) pending writes in memory. I consider this OK again since input is bounded, but this would need to be changed for interactive use.
* I choose `Rope` to represent the current buffer so as to efficiently support all operations
* Though no errors propagate to top level, I don't do anything fancy with reporting for parsing since the format is so simple. This could be improved on to give better hints and line number where parse error occured
* The constraints listed in the website are considered to be guarentees that can be trusted about the input. If we were generating the input programmatically, we should make sure the constraints hold - if we are consuming input from an outside source, we should make sure the constraints hold before evaluation.
* Since input is assumed lowercase English letters, and the input format is not specified, we assume the input format is ascii. This is generally a bad assumption in production but simplifies the implementation here.


For more specific implementation details, see the documentation via `stack haddock`
