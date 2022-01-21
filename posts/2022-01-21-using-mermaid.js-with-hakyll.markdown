---
title: Using mermaid.js with Hakyll
summary: Render mermaid charts from markdown in your Hakyll blog.
tags: hakyll, mermaid, pandoc
---

[Mermaid](https://mermaidjs.github.io/) is a Javascript library that can render
multiple types of diagrams from a fairly simple syntax. For example, this diagram
is an example of a flowchart:

```mermaid
graph TD
    A[Use Hakyll] --> B{Want diagrams}
    B --> D[Hand-drawn]
    B -->|This post| E[Mermaid.js]
    B --> F[Omnigraffle]
```

And this is the markdown used to generate it within this post:

~~~markdown
```mermaid
graph TD
    A[Use Hakyll] --> B{Want diagrams}
    B --> D[Hand-drawn]
    B -->|This post| E[Mermaid.js]
    B --> F[Omnigraffle]
```
~~~

In this blog, every markdown code block with the `mermaid` language is
rendered as a mermaid diagram on the client-side using [mermaid.js](https://mermaidjs.github.io/).
Read on to see how I set that up.

