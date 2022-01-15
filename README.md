# ppj-blog

This is the repository for my blog, http://blog.postpunkjustin.com.

# Development

I've put in some effort to make Codespaces the preferred way to work on this.

At the moment, new Codespaces start with a mostly blank slate, so until I have prebuilt images ready, here are the steps to initialize a new Codespace to be ready to write and publish posts:
1. Get a terminal in the Codespace via VS Code (`gh cs code`). If you use `gh cs ssh`, you won't get automatic port forwarding so the blog preview won't work.
2. Run `stack build`. Stack should already be installed, but this will still take quite a while (I think >10 minutes).
3. Run `stack exec ppj-blog watch` to start the server.

# Publishing

I haven't quite figured this out yet.