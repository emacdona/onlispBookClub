---
layout: post
published: true
title: Spring @Cacheable
cacheable: "`@Cacheable`{:.language-java .highlight}"
cacheevict: "`@CacheEvict`{:.language-java .highlight}"
cacheput: "`@CachePut`{:.language-java .highlight}"
dockercompose: "`docker compose`{:.language-shell .highlight}"
docker: "`docker`{:.language-shell .highlight}"
make: "`make`{:.language-shell .highlight}"
curl: "`curl`{:.language-shell .highlight}"
jq: "`jq`{:.language-shell .highlight}"
perl: "`perl`{:.language-shell .highlight}"
prompt: "`✗`{:.language-shell .highlight}"
clearCache: "`clearCache`{:.language-java .highlight .nf}"
books: "`books`{:.language-java .highlight}"
bookByIsbn: "`bookByIsbn`{:.language-java .highlight}"
badUpdateTitle: "`badUpdateTitle`{:.language-java .highlight}"
betterUpdateTitle: "`betterUpdateTitle`{:.language-java .highlight}"
bestUpdateTitle: "`bestUpdateTitle`{:.language-java .highlight}"
isbn: "`isbn`{:.language-java .highlight}"
booktitle: "`title`{:.language-java .highlight}"
author: "`author`{:.language-java .highlight}"
cached: "`cached`{:.language-java .highlight}"
host: "`host`{:.language-java .highlight}"
---

# W.I.P.[^work-in-progress]: Spring Declarative Caching

## Why this blog post?

I had occasion at work recently to consider using Spring's {{page.cacheable}} annotation. I searched the web for
some hints on how to use it, but wasn't satisfied with any of the results. Even my favorite source[^baeldung]
of bite-size Java code samples had examples that I felt
were [too](https://www.baeldung.com/spring-cache-tutorial#2-cacheevict) [simple](https://www.baeldung.com/spring-cache-tutorial#3-cacheput).
Using "lookup" methods
as examples of places where you may want to put {{page.cacheevict}} and {{page.cacheput}} annotations seemed misleading
to me.

So, at the very least, I wanted to provide more substantial examples. In particular, I wanted to show why Spring's
default cache implementation requires careful thought in
a distributed application environment.

In the process, I ended up going off the rails a bit. I spent way more time setting up an environment in which I could
run my examples than I did writing actual Java code.
But, in a way, that's great! If you came here to read Java code, the good new is: there's not a lot of it.

## Sample Code

For this blog post, I bootstrapped a Spring project using the [spring initializr](https://start.spring.io/) and
then [put it on Github](https://github.com/emacdona/blog-spring-cacheable).

To run the examples with the least amount of fuss, you'll need {{page.docker}}, {{page.dockercompose}}, and (gnu)
{{page.make}}. If you have those tools, things _should_ go smoothly. However,
it _is_ software... so, you know... good luck. If it breaks, you've got the source code!

I'll be using {{page.curl}} and {{page.jq}} (via a Makefile) to exercise the Spring application. Once you fire it up,
though, it will have a Swagger UI
at [http://localhost:8080/swagger-ui/index.html](http://localhost:8080/swagger-ui/index.html) -- you can use that if you
like.

For examples that show a command typed at a prompt, this character is my prompt: {{page.prompt}}

## Big Picture

Broadly speaking, you'd like to cache return values of methods that are expensive to compute. You can measure 
"expensive" in different ways (eg: CPU or memory used); but often, when
we say "expensive", we mean "it takes too much time".

If we assume that all methods in question are "functions" in the mathematical sense -- ie: given the same inputs (to
include the instance the method is operating on), they always give the same output -- then caching is pretty simple. Any 
return value of such a method can be cached indefinitely, and the only decisions you need to make with respect to your 
cache are:
1. How big can it get?
2. Which eviction policy do you choose to ensure the size constraint is maintained?

If we do ***not*** assume that the method in question are functions, then we can consider much more interesting examples.

For instance: consider a service that, given an identifier, retrieves a record from a relational database. Assume that this
database has multiple clients, each of which can update records. Should you expect that, given the same identifier, the service
should return the same record now that it did six hours ago? Of course not: the record could have been updated in the 
intervening time.

## Sample Application

The sample application includes a very simple service for managing a database of books. It uses Spring declarative 
caching to minimize database lookups. It lets you:
1. Retrieve books
2. Change books' titles

Following is a stripped down version of the RestController class that implements the API endpoints for our service (see the
source code for the complete class). 

```java
@RequestMapping("/books")
public class BookRestController {
  private final BookRepository bookRepository;

  @GetMapping("/clear")
  @CacheEvict(cacheNames = "books", allEntries = true)
  public void clearCache() {
  }

  @GetMapping
  public Collection<Book> books() {
    return bookRepository.findAll();
  }

  @GetMapping("/{isbn}")
  @Cacheable(cacheNames = "books")
  public Book bookByIsbn(@PathVariable("isbn") String isbn) {
    return bookRepository.findByIsbn(isbn);
  }

  @GetMapping("/{isbn}/badUpdateTitle/{title}")
  public Book badUpdateTitle(@PathVariable("isbn") String isbn,
                             @PathVariable("title") String title) {
    return bookRepository.save(bookRepository.findByIsbn(isbn).withTitle(title));
  }

  @GetMapping("/{isbn}/betterUpdateTitle/{title}")
  @CacheEvict(cacheNames = "books", key = "#isbn")
  public Book betterUpdateTitle(@PathVariable("isbn") String isbn,
                                @PathVariable("title") String title) {
    return bookRepository.save(bookRepository.findByIsbn(isbn).withTitle(title));
  }

  @GetMapping("/{isbn}/bestUpdateTitle/{title}")
  @CachePut(cacheNames = "books", key = "#isbn")
  public Book bestUpdateTitle(@PathVariable("isbn") String isbn,
                              @PathVariable("title") String title) {
    return bookRepository.save(bookRepository.findByIsbn(isbn).withTitle(title));
  }
}
```

Some notes on the methods and their use of Spring's declarative caching.

* {{page.clearCache}}: This method is for testing purposes only! It uses {{page.cacheevict}} to clear ***ALL*** items from the cache. It allows us to start from a known state (empty cache) when we are testing. In a production application, however, I can't imagine a scenario when you'd want to clear the entire cache -- this is one of the reasons I didn't like examples I found on the web: this seemed to be the most popular example use of this annotation.
* {{page.books}}: This method returns a list of all Books. It does not cache results. It's a convenience method that lets us see all Books. There are interesting questions here that I chose to completely ignore (but they are worth further research):
  * If we were to cache the result, would it cache the list as a unit, or the items in the list individually?
  * If it does cache the list as a unit (which I think is the case), how could we make it cache the results individually?
  * If the list were cached as a unit, how would we manage the cache when an individual Book was updated? Would we clear ***ALL*** cached lists of books (in case it appears in one of them)?
* {{page.bookByIsbn}}: Given an isbn, this method returns a single Book and caches the result ({{page.cacheable}}).
* {{page.badUpdateTitle}}: Given an isbn and a title, this method will update the title of the Book determined by the isbn. This is "bad" because it allows for records in the database to be updated without correpsonding records in the cache being updated.
* {{page.betterUpdateTitle}}: Given an isbn and a title, this method will update the the title of the Book determined by the isbn _and_ _evict_ any instance of the book from the cache ({{page.cacheevict}}). This is "better" because when a database record is updated, any corresponding record in the cache is removed.
* {{page.bestUpdateTitle}}: Given an isbn and a title, this method will update the title of the book determined by the isbn _and_ _update_ any instance of the book from the cache ({{page.cacheput}}). This is "best" because when a database record is updated, any corresponding record in the cache is also updated.

A stripped down version of the Book entity follows (see the source for the complete class).

```java
public class Book {
  private String isbn;
  private String title;
  private String author;
  private Boolean cached;
  private String host;
}
```

This entity captures the {{page.isbn}}, {{page.bookTitle}}, and {{page.author}} of the book -- no surprises there.

However, it also has two additional fields: {{page.cached}} and {{page.host}}. These two fields aren't actually stored
in the database. They are managed by some clever AspectJ code. For a given instance of a book, they allow us to see:
* Whether it came from a cache.
* Which host provided it in response to our request (which is interesting when we deploy multiple instances of our application).

The same sample application can be deployed multiple ways, each having an impact on caching behavior.

### Single JVM

The simplest of these architectures is a single instance of the application, using both an in-memory cache and an in-memory 
database (all in the same JVM):

```mermaid!
block-beta
  columns 4
  client["client"]
  space
  block:group1:2
    columns 6
    space app(["app"]) space cache[("cache\n(in mem)")] space db[("db\n(H2)")] 
    app --> cache
    cache --> db
  end
  client --> app
```

In this configuration, all requests are answered by the same application instance. That instance has a single backend
database. Its responses are saved in a single cache.

The following sequence diagram shows what happens when we attempt to retrieve a book. The Cache Interceptor intercepts the
request and first checks to see if the result is already in the cache. If it is, the interceptor returns the result.
If it's not, the interceptor calls the Endpoint Method and adds the result to the cache before returning it to the Client.

```mermaid!
sequenceDiagram
    participant B as Client
    box rgba(98, 175, 192, 0.5) Single JVM
    participant CI as Cache Interceptor
    participant EM as Endpoint Method
        participant C as Cache
    participant DB as Database
    end
    B->>CI: GET
    CI->>C: GET
    alt cache miss
        CI->>EM: GET
        EM->>DB:SELECT
        DB-->>EM: return
        EM-->>CI: return
        CI->>C: PUT
        CI-->>B: return
    else cache hit
        C-->>CI: return
        CI-->>B: return
        end
```

To deploy the application in this configuration, run the following command (from the project's root directory):

```shell
✗ make single-instance
```

Now let's kick the tires! Open another shell (because the one in which you ran the previous command should now be occupied).

Note that if we clear the cache and then make 99 requests for the same book, 98 requests are served by the cache[^make-automation]:

```shell
✗ make clear-cache get-book
curl -s http://localhost:8080/books/clear;
for i in {1..99};
do
curl -s http://localhost:8080/books/0130305529 | jq -c '. | {host, cached,title}';
done \
| sort | uniq -c
      1 {"host":"454fe726c47c","cached":false,"title":"On Lisp"}
     98 {"host":"454fe726c47c","cached":true,"title":"On Lisp"}
```

Then, if we (without clearing caches) "bad" update the title and then make 99 requests for the book... all 99 requests
are served by the cache, and all 99 have the ***wrong*** (old) title:

```shell
✗ make bad-update-title get-book 
curl -s http://localhost:8080/books/0130305529/badUpdateTitle/"HELLO%20WORLD"%20BAD | jq -c '.'
{"isbn":"0130305529","title":"HELLO WORLD BAD","author":"Paul Graham","cached":false,"host":"454fe726c47c"}
for i in {1..99};
do
curl -s http://localhost:8080/books/0130305529 | jq -c '. | {host, cached,title}';
done \
| sort | uniq -c
     99 {"host":"454fe726c47c","cached":true,"title":"On Lisp"}
```

The lesson to be learned here is that you are responsible for keeping the cache up to date with the "source of truth". In our
example, the "source of truth" is our database.

Next, if we (again, without clearing caches) "better" update the title and then make 99 requests for the book... 98 requests
are served by the cache, and one is not. However all 99 have the ***correct*** (new) title:
```shell
✗ make better-update-title get-book 
curl -s http://localhost:8080/books/0130305529/betterUpdateTitle/"HELLO%20WORLD"%20BETTER | jq -c '.'
{"isbn":"0130305529","title":"HELLO WORLD BETTER","author":"Paul Graham","cached":false,"host":"454fe726c47c"}
for i in {1..99};
do
curl -s http://localhost:8080/books/0130305529 | jq -c '. | {host, cached,title}';
done \
| sort | uniq -c
      1 {"host":"454fe726c47c","cached":false,"title":"HELLO WORLD BETTER"}
     98 {"host":"454fe726c47c","cached":true,"title":"HELLO WORLD BETTER"}
```

***This*** is a much better example of how to use {{page.cacheevict}} (in my opinion) than I was able to find online. Any time we update a record in the database, we evict
any instances of it in the cache. This is much better than evicting the entire cache.

Finally, if we (again, without clearing caches) "best" update the title and then make 99 requests for the book... all 99 requests
are served by the cache, and all 99 have the ***correct*** (new) title:
```shell
✗ make best-update-title get-book 
curl -s http://localhost:8080/books/0130305529/bestUpdateTitle/"HELLO%20WORLD"%20BEST | jq -c '.'
{"isbn":"0130305529","title":"HELLO WORLD BEST","author":"Paul Graham","cached":false,"host":"454fe726c47c"}
for i in {1..99};
do
curl -s http://localhost:8080/books/0130305529 | jq -c '. | {host, cached,title}';
done \
| sort | uniq -c
     99 {"host":"454fe726c47c","cached":true,"title":"HELLO WORLD BEST"}
```

This example is arguably the best method for keeping a cache up to date. Any time a record is updated in the database, we add it to the cache (if it wasn't there already) or
update it in the cache (if it was already there). This allows us to serve as many lookup requests from the cache as possible.

If you want to keep playing, you can use the following command to restore the state to what it was before we started. However,
if you plan to just move on to the examples in the next session, there is no need -- because we are going to bring down the
entire application and deploy it in a new configuration.

```shell
✗ make restore-title-for-all-replicas clear-cache-for-all-replicas
```


### Replicas with Individual Caches, Shared Database

A misguided next step in the evolution of this application's architecture (especially if you think the way I do) would be
to create multiple replicas of the app that all store their data in a single database -- ***without*** also centralizing
the cache.

```mermaid!
block-beta
  %% columns auto (default)
  
  block:clientblock:1
    columns 1
    space
    space
    client["client"]
    space
    space
  end
  
  style clientblock fill:#fce1c5, stroke: #fce1c5
  
  block:lbblock:1
    columns 1
    space
    space
    lb["load\nbalancer"]:1
    space
    space
  end
  
  style lbblock  fill:#fce1c5, stroke: #fce1c5
  
  block:replicas:1
    columns 2
  
    block:replica1:2
      app1(["app"]) cache1[("cache\n(in mem)")]
      app1 --> cache1
    end
    
    block:replica2:2
      app2(["app"]) cache2[("cache\n(in mem)")]
      app2 --> cache2
    end
    
    block:replica3:2
      app3(["app"]) cache3[("cache\n(in mem)")]
      app3 --> cache3
    end
    
    block:replica4:2
      app4(["app"]) cache4[("cache\n(in mem)")]
      app4 --> cache4
    end
    
    block:replica5:2
      app5(["app"]) cache5[("cache\n(in mem)")]
      app5 --> cache5
    end
    
  end
  
  client --> lb
  lb --> app1
  lb --> app2
  lb --> app3
  lb --> app4
  lb --> app5
  
  db[("db\n(Postgres)")]:1
  
  cache1 --> db
  cache2 --> db
  cache3 --> db
  cache4 --> db
  cache5 --> db
```

As you can see from the following sequence diagram, caches still belong to individual replicas, and are updated independently of one another.
If a replica answers a given retrieval request, only its cache is populated with the value. If a given replica services an update request,
only its cache is updated with the new value. Any other replica that has cached the value previously will now return stale data for lookup requests!

```mermaid!
sequenceDiagram
    participant B as Client
    box rgba(98, 175, 192, 0.5) Replica 1
        participant CI as Cache Interceptor
        participant EM as Endpoint Method
        participant C as Cache
    end
    box rgba(98, 175, 192, 0.5) Replica 2
        participant CI2 as Cache Interceptor
        participant EM2 as Endpoint Method
        participant C2 as Cache
    end
    box rgba(98, 175, 192, 0.5) Database Server
        participant DB as Database
    end
    B->>CI: GET
    CI->>C: GET
    alt cache miss
        CI->>EM: GET
        EM->>DB:SELECT
        DB-->>EM: return
        EM-->>CI: return
        CI->>C: PUT
        CI-->>B: return
    else cache hit
        C-->>CI: return
        CI-->>B: return
    end
```

```shell
✗ make clean
```

```shell
✗ make replicas-individual-cache-shared-db
```

```shell
✗ make clear-cache get-book
curl -s http://localhost:8080/books/clear;
for i in {1..99};
do
curl -s http://localhost:8080/books/0130305529 | jq -c '. | {host, cached,title}';
done \
| sort | uniq -c
      1 {"host":"0ab9998805ab","cached":false,"title":"On Lisp"}
     19 {"host":"0ab9998805ab","cached":true,"title":"On Lisp"}
      1 {"host":"0ad497034e6e","cached":false,"title":"On Lisp"}
     19 {"host":"0ad497034e6e","cached":true,"title":"On Lisp"}
      1 {"host":"42b5ae496324","cached":false,"title":"On Lisp"}
     19 {"host":"42b5ae496324","cached":true,"title":"On Lisp"}
      1 {"host":"5ea86506da53","cached":false,"title":"On Lisp"}
     19 {"host":"5ea86506da53","cached":true,"title":"On Lisp"}
      1 {"host":"7cfe13ae6a5c","cached":false,"title":"On Lisp"}
     18 {"host":"7cfe13ae6a5c","cached":true,"title":"On Lisp"}
```

```shell
✗ make bad-update-title get-book
curl -s http://localhost:8080/books/0130305529/badUpdateTitle/"HELLO%20WORLD"%20BAD | jq -c '.'
{"isbn":"0130305529","title":"HELLO WORLD BAD","author":"Paul Graham","cached":false,"host":"7cfe13ae6a5c"}
for i in {1..99};
do
curl -s http://localhost:8080/books/0130305529 | jq -c '. | {host, cached,title}';
done \
| sort | uniq -c
     20 {"host":"0ab9998805ab","cached":true,"title":"On Lisp"}
     20 {"host":"0ad497034e6e","cached":true,"title":"On Lisp"}
     20 {"host":"42b5ae496324","cached":true,"title":"On Lisp"}
     20 {"host":"5ea86506da53","cached":true,"title":"On Lisp"}
     19 {"host":"7cfe13ae6a5c","cached":true,"title":"On Lisp"}
```

```shell
✗ make better-update-title get-book
curl -s http://localhost:8080/books/0130305529/betterUpdateTitle/"HELLO%20WORLD"%20BETTER | jq -c '.'
{"isbn":"0130305529","title":"HELLO WORLD BETTER","author":"Paul Graham","cached":false,"host":"7cfe13ae6a5c"}
for i in {1..99};
do
curl -s http://localhost:8080/books/0130305529 | jq -c '. | {host, cached,title}';
done \
| sort | uniq -c
     20 {"host":"0ab9998805ab","cached":true,"title":"On Lisp"}
     20 {"host":"0ad497034e6e","cached":true,"title":"On Lisp"}
     20 {"host":"42b5ae496324","cached":true,"title":"On Lisp"}
     20 {"host":"5ea86506da53","cached":true,"title":"On Lisp"}
      1 {"host":"7cfe13ae6a5c","cached":false,"title":"HELLO WORLD BETTER"}
     18 {"host":"7cfe13ae6a5c","cached":true,"title":"HELLO WORLD BETTER"}
```

```shell
✗ make best-update-title get-book
curl -s http://localhost:8080/books/0130305529/bestUpdateTitle/"HELLO%20WORLD"%20BEST | jq -c '.'
{"isbn":"0130305529","title":"HELLO WORLD BEST","author":"Paul Graham","cached":false,"host":"7cfe13ae6a5c"}
for i in {1..99};
do
curl -s http://localhost:8080/books/0130305529 | jq -c '. | {host, cached,title}';
done \
| sort | uniq -c
     20 {"host":"0ab9998805ab","cached":true,"title":"On Lisp"}
     20 {"host":"0ad497034e6e","cached":true,"title":"On Lisp"}
     20 {"host":"42b5ae496324","cached":true,"title":"On Lisp"}
     20 {"host":"5ea86506da53","cached":true,"title":"On Lisp"}
     19 {"host":"7cfe13ae6a5c","cached":true,"title":"HELLO WORLD BEST"}
```

```shell
✗ make clear-cache get-book
```

```shell
✗ make restore-title-for-all-replicas clear-cache-for-all-replicas
```

### Replicas with Shared Caches, Shared Database

```mermaid!
block-beta
  %% columns auto (default)
  
  block:clientblock:1
    columns 1
    space
    space
    client["client"]
    space
    space
  end
  
  style clientblock fill:#fce1c5, stroke: #fce1c5
  
  block:lbblock:1
    columns 1
    space
    space
    lb["load\nbalancer"]:1
    space
    space
  end
  
  style lbblock  fill:#fce1c5, stroke: #fce1c5
  
  block:replicas:1
    columns 1
  
    block:replica1:1
      app1(["app"])
    end
    
    block:replica2:1
      app2(["app"])
    end
    
    block:replica3:1
      app3(["app"])
    end
    
    block:replica4:1
      app4(["app"])
    end
    
    block:replica5:1
      app5(["app"])
    end
    
  end
  
  client --> lb
  lb --> app1
  lb --> app2
  lb --> app3
  lb --> app4
  lb --> app5
  
  app1 --> cache
  app2 --> cache
  app3 --> cache
  app4 --> cache
  app5 --> cache
  
  cache[("cache\n(Redis)")]:1
  
  db[("db\n(Postgres)")]:1
  
  cache --> db
```

```mermaid!
sequenceDiagram
participant B as Client
box rgba(98, 175, 192, 0.5) Replica 1
participant CI as Cache Interceptor
participant EM as Endpoint Method
end
box rgba(98, 175, 192, 0.5) Replica 2
participant CI2 as Cache Interceptor
participant EM2 as Endpoint Method
end
box rgba(98, 175, 192, 0.5) Cache Server
participant C as Cache
end
box rgba(98, 175, 192, 0.5) Database Server
participant DB as Database
end
B->>CI: GET
CI->>C: GET
alt cache miss
CI->>EM: GET
EM->>DB:SELECT
DB-->>EM: return
EM-->>CI: return
CI->>C: PUT
CI-->>B: return
else cache hit
C-->>CI: return
CI-->>B: return
end
```

```shell
✗ make clean
```

```shell
✗ make replicas-shared-cache-shared-db
```

```shell
✗ make clear-cache get-book
curl -s http://localhost:8080/books/clear;
for i in {1..99};
do
curl -s http://localhost:8080/books/0130305529 | jq -c '. | {host, cached,title}';
done \
| sort | uniq -c
     20 {"host":"2e989a481029","cached":true,"title":"On Lisp"}
     20 {"host":"6353b2b26113","cached":true,"title":"On Lisp"}
     19 {"host":"885eb7e3679c","cached":true,"title":"On Lisp"}
     20 {"host":"9ad678f720d2","cached":true,"title":"On Lisp"}
      1 {"host":"fbc281a88ef7","cached":false,"title":"On Lisp"}
     19 {"host":"fbc281a88ef7","cached":true,"title":"On Lisp"}
```

```shell
✗ make bad-update-title get-book
curl -s http://localhost:8080/books/0130305529/badUpdateTitle/"HELLO%20WORLD"%20BAD | jq -c '.'
{"isbn":"0130305529","title":"HELLO WORLD BAD","author":"Paul Graham","cached":false,"host":"885eb7e3679c"}
for i in {1..99};
do
curl -s http://localhost:8080/books/0130305529 | jq -c '. | {host, cached,title}';
done \
| sort | uniq -c
     20 {"host":"2e989a481029","cached":true,"title":"On Lisp"}
     20 {"host":"6353b2b26113","cached":true,"title":"On Lisp"}
     19 {"host":"885eb7e3679c","cached":true,"title":"On Lisp"}
     20 {"host":"9ad678f720d2","cached":true,"title":"On Lisp"}
     20 {"host":"fbc281a88ef7","cached":true,"title":"On Lisp"}
```

```shell
✗ make better-update-title get-book
curl -s http://localhost:8080/books/0130305529/betterUpdateTitle/"HELLO%20WORLD"%20BETTER | jq -c '.'
{"isbn":"0130305529","title":"HELLO WORLD BETTER","author":"Paul Graham","cached":false,"host":"885eb7e3679c"}
for i in {1..99};
do
curl -s http://localhost:8080/books/0130305529 | jq -c '. | {host, cached,title}';
done \
| sort | uniq -c
     20 {"host":"2e989a481029","cached":true,"title":"HELLO WORLD BETTER"}
     20 {"host":"6353b2b26113","cached":true,"title":"HELLO WORLD BETTER"}
     19 {"host":"885eb7e3679c","cached":true,"title":"HELLO WORLD BETTER"}
     20 {"host":"9ad678f720d2","cached":true,"title":"HELLO WORLD BETTER"}
      1 {"host":"fbc281a88ef7","cached":false,"title":"HELLO WORLD BETTER"}
     19 {"host":"fbc281a88ef7","cached":true,"title":"HELLO WORLD BETTER"}
```

```shell
✗ make best-update-title get-book
curl -s http://localhost:8080/books/0130305529/bestUpdateTitle/"HELLO%20WORLD"%20BEST | jq -c '.'
{"isbn":"0130305529","title":"HELLO WORLD BEST","author":"Paul Graham","cached":false,"host":"885eb7e3679c"}
for i in {1..99};
do
curl -s http://localhost:8080/books/0130305529 | jq -c '. | {host, cached,title}';
done \
| sort | uniq -c
     20 {"host":"2e989a481029","cached":true,"title":"HELLO WORLD BEST"}
     20 {"host":"6353b2b26113","cached":true,"title":"HELLO WORLD BEST"}
     19 {"host":"885eb7e3679c","cached":true,"title":"HELLO WORLD BEST"}
     20 {"host":"9ad678f720d2","cached":true,"title":"HELLO WORLD BEST"}
     20 {"host":"fbc281a88ef7","cached":true,"title":"HELLO WORLD BEST"}
```

```shell
✗ make clear-cache get-book
```

```shell
✗ make restore-title-for-all-replicas clear-cache-for-all-replicas
```
<!---@formatter:off--->
[^draft]: While this is in "draft" status, there may be significant changes. Such changes should not be expected to have correction notes attached.

[^work-in-progress]: A hastily organized mess with large chunks simply unwritten. Published so I can show people the direction I am taking.

[^baeldung]: Seriously, I'm not picking on Baeldung. It's one of my favorite web sites. Even though I didn't love its examples in this instance, the article I referenced served as
    one of the starting points for this very blog post.

[^make-automation]: The Makefile contains targets that allow me to run commands to exercise the application more succinctly.
    {{page.make}} also has the wonderful feature of echoing commands that it runs. This allows me to type a short command and
    copy paste the output directly into this post -- showing you both the commands that were actually run and the output that they
    generated.

[^jqshowoff]: You could just look at the Swagger UI, which shows these descriptions. But I said I was going to use 
    {{page.curl}} and {{page.jq}}. I'm committed.
<!---@formatter:on--->
