---
title: What you should know about MySQL's LAST_INSERT_ID()
summary: Why it's useful, but more importantly how to avoid it.
tags: mysql, databases, sql, last_insert_id
---

## how I got into this mess

If you're writing MySQL without the "benefit" of an
[ORM](https://en.wikipedia.org/wiki/Object-relational_mapping), you may find
yourself in a situation where you need to know the automatically generated ID of
the last inserted row.

More civilized databases like Postgres let you specify in the INSERT statement
what you want to be returned, but MySQL still hasn't made it that easy. When I
came across this, the advice I found all over the internet was to use the built-in
function `LAST_INSERT_ID()` to get the ID of the last inserted row.

Frankly, that rubs me the wrong way. A couple of issues come to mind immediately:

1. What if something else gets inserted before I manage to call `LAST_INSERT_ID()`?
2. Why should I have to incur the cost of another entire query just to get the ID?

## can I trust it?

For problem 1, the [official docs](https://dev.mysql.com/doc/refman/8.0/en/information-functions.html#function_last-insert-id) are somewhat reassuring:

> The ID that was generated is maintained in the server on a ***per-connection*** basis. This means that the value returned by the function to a given client is the first `AUTO_INCREMENT` value generated for most recent statement affecting an `AUTO_INCREMENT` column ***by that client***. This value cannot be affected by other clients, even if they generate `AUTO_INCREMENT` values of their own. This behavior ensures that each client can retrieve its own ID without concern for the activity of other clients, and without the need for locks or transactions. 

Translation: "don't worry, we manage the implicit state here so you don't have
to." And sure, you're already trusting MySQL with your data, so why not trust it
to return the correct ID?

Here's a sequence diagram showing how `last_insert_id()` works with multiple clients,
according to the documentation:

```mermaid
sequenceDiagram
    actor Alice
    participant DB as MySQL
    actor Bob
    Alice->>DB: INSERT a
    Note right of Alice: a.id = 1
    Bob->>DB: INSERT b
    Note left of Bob: b.id = 2
    Alice->>DB: SELECT last_insert_id()
    DB-->>Alice: last_insert_id = 1
    Bob->>DB: SELECT last_insert_id()
    DB-->>Bob: last_insert_id = 2
```

### what about Vitess?

But what if you _can't_ actually trust the assumptions made by MySQL? This isn't a
hypothetical. If you're using [Vitess](https://vitess.io/), then you should be ready
for subtle differences compared to real MySQL. I found this somewhat alarming tidbit
in the [Vitess docs](https://vitess.io/docs/12.0/concepts/query-rewriting/#connection-pooling)
during my research:

> When a tablet talks with a MySQL to execute a query on behalf of a user, it does not use a dedicated connection per user, and instead will share the underlying connection between users. This means that it’s not safe to store any state in the session as you can’t be sure it will continue executing queries on the same connection, and you can’t be sure if this connection will be used by other users later on.

At first glance, that looks like it completely invalidates the assumptions made by `LAST_INSERT_ID()`.
If Vitess is sharing connections and splitting queries across them, then it stands to reason that it
could get an ID from a different connection than the one used to execute the query.

Older versions of Vitess did have some limitations here, but luckily that has been fixed.
The `LAST_INSERT_ID()` function is important enough to have been treated as [a special case in Vitess](https://github.com/vitessio/vitess/issues/3668).
Queries that use `LAST_INSERT_ID()` are rewritten to instead get the value in a safe, reliable way
from vtgate.

```mermaid
sequenceDiagram
    actor Alice
    actor Bob
    participant vt as vtgate
    participant DB as MySQL
    Alice->>vt: INSERT c
    vt->>DB: INSERT c
    DB-->>vt: last_insert_id = 3
    Bob->>vt: INSERT d
    vt->>DB: INSERT d
    DB-->>vt: last_insert_id = 4
    Alice->>vt: SELECT last_insert_id()
    vt-->>Alice: last_insert_id = 3
    Bob->>vt: SELECT last_insert_id()
    vt-->>Bob: last_insert_id = 4
```

As long as you're not using a truly ancient version of Vitess, you should be fine.

### what's the worst-case scenario?

As an experiment, I did actually try to write an example service that got the wrong results
from `LAST_INSERT_ID()`. It's not impossible. What you have to do is go out of your way to
share an open connection object between multiple threads that are each making inserts and retrieving
the new IDs.

Most of the time, this actually causes threads to crash as they fight over
active connections and eventually get only errors back, but sometimes I did in fact get
the wrong IDs returned.

I think it's pretty unlikely that someone is going to write that code accidentally, and if they do then
the rampant crashing will probably get them to reexamine it pretty quickly.

## but I still want to avoid it

So far I've convinced myself (if not also you) that `LAST_INSERT_ID()` _works_ about as well as they say it will.
But that doesn't mean I have to like it! I still feel annoyed that I need to make another round-trip call to the database
to get the ID. Shouldn't there be another way?

### the OK_Packet

It turns out that the answer is part of the [MySQL protocol](https://dev.mysql.com/doc/dev/mysql-server/latest/page_protocol_basics.html)
rather than SQL queries. Successful queries don't _just_ have a results set that gets exposed to the client,
they also come bound with the [OK_Packet](https://dev.mysql.com/doc/dev/mysql-server/latest/page_protocol_basic_ok_packet.html). 
This is the packet that contains the ID of the last inserted row.

A decent MySQL client library will parse this packet and make the ID available to you. Look at the various query functions/methods
that it exposes and find one that returns the `last_insert_id` property. If you don't find one, then either find another library
or roll up your sleeves and make a pull request to add it.

## conclusion

The `LAST_INSERT_ID()` function is probably fine to use, but you can and should
avoid it anyway. It's certainly not going to make your code _more_ reliable, and
it has a needless performance cost. Instead, get the `last_insert_id` from the
OK_Packet via your client library.