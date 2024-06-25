---
layout: post
published: false
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

# DRAFT[^draft]: Spring Caching

## Why this blog post?

I had occasion at work recently to consider using Spring's {{page.cacheable}} annotation. I searched the web for
some hints on how to use it, but wasn't satisfied with any of the results. Even my favorite source[^baeldung]
of bite-size Java code samples had examples that I felt
were [too](https://www.baeldung.com/spring-cache-tutorial#2-cacheevict) [simple](https://www.baeldung.com/spring-cache-tutorial#3-cacheput).
Using "get" methods
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
* {{page.badUpdateTitle}}: Given an isbn and a title, this method will update the title of the Book determined by the isbn. It does NOT make any updates to the cache!
* {{page.betterUpdateTitle}}: Given an isbn and a title, this method will update the the title of the Book determined by the isbn _and_ _evict_ any instance of the book from the cache ({{page.cacheevict}}).
* {{page.bestUpdateTitle}}: Given an isbn and a title, this method will update the title of the book determined by the isbn _and_ _update_ any instance of the book from the cache ({{page.cacheput}}).

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
    participant B as Browser
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

### Replicas with Individual Caches, Shared Database

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

```mermaid!
sequenceDiagram
    participant B as Browser
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
participant B as Browser
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

<!---@formatter:off--->
[^draft]: While this is in "draft" status, there may be significant changes. Such changes should not be expected to have correction notes attached.

[^baeldung]: Seriously, I'm not picking on Baeldung. It's one of my favorite web sites. Even though I didn't love its examples in this instance, the article I referenced served as
    one of the starting points for this very blog post.

[^jqshowoff]: You could just look at the Swagger UI, which shows these descriptions. But I said I was going to use 
    {{page.curl}} and {{page.jq}}. I'm committed.
<!---@formatter:on--->
