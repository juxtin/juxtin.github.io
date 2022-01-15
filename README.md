# ppj-blog

This is the repository for my blog, http://blog.postpunkjustin.com.

# Development

I've put in some effort to make Codespaces the preferred way to work on this.

At the moment, new Codespaces start with a mostly blank slate and I haven't set up prebuilt images yet, so creating a new Codespace takes something like 10+ minutes. Once that's done:
1. Get a terminal in the Codespace via VS Code (`gh cs code`). If you use `gh cs ssh`, you won't get automatic port forwarding so the blog preview won't work.
2. Run `script/watch` to start the server in preview/developer mode.

# Publishing

The deploy process is automated. Run `script/deploy` and it should make a new commit to the gh-pages branch with the changed content.