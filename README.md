# HELLO

This is my blog. 

## Technical details and licensing

Currently, the blog is built with [Hakyll 4.12](http://hackage.haskell.org/package/hakyll) and [Stack LTS 12.13](https://www.stackage.org/lts-12.13) (therefore, GHC 8.4.3). You can find the complete dependency list in the `.cabal` file. The Haskell code is all in `site.hs`, the html templates are all in the `templates` directory, and the CSS is all in the `default.css` file. That CSS includes the syntax highlighting in code blocks.

All code, including HTML and CSS, are licensed under [The Unlicense](http://unlicense.org/). Please feel free to use any of it to start your own blog relatively painlessly. 

My blog is hosted on AWS. You can find some instructions for setting that up [here](https://osterlund.xyz/posts/2015-11-06-hakyll_site_amazon_aws.html). I plan to write a guide to setting up your first static site with Hakyll, from intial build to deployment, at some point, but I haven't yet. I chose to use AWS initially because I wanted some experience with AWS; now I just keep it there out of habit. There is also a tutorial for hosting a [Hakyll site on Github Pages](https://jaspervdj.be/hakyll/tutorials/github-pages-tutorial.html) but I haven't personally tried it.

## Content license

The content in this blog, i.e. all images, text, and other assets, included in blog posts and pages, are licensed under a [Creative Commons Attribution-NonCommercial 4.0 International License](https://creativecommons.org/licenses/by-nc/4.0/). You are free to use them with attribution for any noncommercial purpose. You may adapt and build upon them as you see fit for your audience. 


## Why Hakyll

I like Hakyll a lot. It was one of my first Haskell projects, to get a Hakyll site going, and so there is undoubtedly some sentimental attachment to it. But at this point I've been involved in building so many Hakyll sites, and I've enjoyed the relative simplicity and flexibility of it. The [Pandoc](https://pandoc.org/MANUAL.html) integration means you have much more flexibility for text presentation than you probably realize. [The Joy of Haskell site](https://joyofhaskell.com/) is built with Hakyll, and we've been really happy with it.

The Hakyll author/maintainer, Jasper, is super helpful and has written and organized a ton of documentation about Hakyll and happily answers user questions. 

Hakyll is, in my opinion, a good way to start playing with doing something real in Haskell if you're new to it. For the most part, the Haskell code you'll be playing with isn't too wild and you won't need to be adept with monad transformers. 

In addition to Jasper's rather [extensive documentation and tutorials](https://jaspervdj.be/hakyll/), there are other resources you might want to be aware of. 
 
- These two articles provide a good overview of how Hakyll works and how some of the pieces fit together. They are aimed at slightly different audiences, and both are written by experienced Haskellers so I found the perspective really valuable when I was starting.

  * [Hakyll setup](http://yannesposito.com/Scratch/en/blog/Hakyll-setup/)   
  * [Opinionated Hakyll Tutorial](http://sigkill.dk/writings/guides/hakyll.html)  

- Stephen Diehl made a [Hakyll + Bootstrap quickstart package](https://github.com/sdiehl/hakyll-bootstrap).  
 
- Dr. Kat Chuang maintains a [Hakyll CSS garden](http://katychuang.com/hakyll-cssgarden/gallery/) with instructions for quickly setting up various themes. That's how I got mine started back when I didn't know anything at all about CSS.  


 