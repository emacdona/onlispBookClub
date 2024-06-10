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
---

# Spring Caching

## Why this blog post?

I had occasion at work recently to consider using Spring's {{page.cacheable}} annotation. I searched the web for 
some hints on how to use it, but wasn't satisfied with any of the results. Even my favorite source[^baeldung]
of bite-size Java code samples had examples that I felt were [too](https://www.baeldung.com/spring-cache-tutorial#2-cacheevict) [simple](https://www.baeldung.com/spring-cache-tutorial#3-cacheput). Using "get" methods
as examples of places where you may want to put {{page.cacheevict}} and {{page.cacheput}} annotations seemed misleading to me.

So, at the very least, I wanted to provide more substantial examples. In particular, I wanted to show why Spring's default cache implementation requires careful thought in 
a distributed application environment.

In the process, I ended up going off the rails a bit. I spent way more time setting up an environment in which I could run my examples than I did writing actual Java code.

## Sample Code

For this blog post, I bootstrapped a Spring project using the [spring initializr](https://start.spring.io/) and then [put it on Github](https://github.com/emacdona/blog-spring-cacheable).

To run the examples with the least amount of fuss, you'll need {{page.docker}}, {{page.dockercompose}}, and {{page.make}}. If you have those tools, things _should_ go smoothly. However,
it _is_ software... so, you know... good luck. If it breaks, you've got the source code!

I'll be using {{page.curl}} and {{page.jq}} to exercise the Spring application. Once you fire it up, though, it will have a Swagger UI at [http://localhost:8080/swagger-ui/index.html](http://localhost:8080/swagger-ui/index.html) -- you can use that if you like.

For examples that show a command typed at a prompt, this character is my prompt: {{page.prompt}}

## Big Picture

Broadly speaking, you'd like to cache return values of methods that are expensive to compute. You can measure "expensive" different ways (eg: CPU or memory used); but often, when
we say "expensive", we mean "it takes too much time".

If we assume that all methods in question are "functions" in the mathematical sense -- ie: given the same inputs (to include the instance the method is operating on), they will generate the
same output [^function]...

...then you can classify methods that "take too much time" into two groups:
1. those without side effects
2. those with side effects

The problem with examples that cache results of methods without side effects is that they are really boring! For a method without side effects, there is no risk in caching its value forever.
The only reason to evict things from the cache is to manage cache size -- you don't have to worry about stale values.

Methods with side effects are more interesting -- 

## Without Side Effects

First we show that what I will call the "canonical" recursive implementation of the Fibonacci sequence is terribly slow. It duplicates an _enormous_ amount of work.

Start up the application by opening a terminal and running:
```shell
✗ make single-instance
```


```shell
✗ time curl -sX 'GET' 'http://localhost:8080/fibonacci/slow/35'  | jq '.'
{
  "result": 9227465,
  "callCount": 18454928,
  "cached": false,
  "host": "89f3a297fc1d"
}
curl -sX 'GET' 'http://localhost:8080/fibonacci/slow/35'  0.01s user 0.00s system 0% cpu 39.489 total
jq '.'  0.02s user 0.00s system 0% cpu 39.489 total
```

Next, we should that using {{page.cacheable}} to mimoize a recursive function _greatly_ reduces the duplicated work done by the "canonical" implementation.

```shell
✗ time curl -sX 'GET' 'http://localhost:8080/fibonacci/fast/35'  | jq '.'
{
  "result": 9227465,
  "callCount": 34,
  "cached": false,
  "host": "89f3a297fc1d"
}
curl -sX 'GET' 'http://host:8080/fibonacci/fast/35'  0.01s user 0.00s system 14% cpu 0.042 total
jq '.'  0.03s user 0.00s system 60% cpu 0.042 total
```

Next, we show that using {{page.cacheable}} to cache the final return value of the function makes even _less_ work for us on
successive calls.

```shell
✗ curl -sX 'GET' 'http://host:8080/books'  | jq -c '.[] | {isbn, title}'
{"isbn":"0130305529","title":"On Lisp"}
{"isbn":"0923891579","title":"Introduction to Meta-Mathematics"}
{"isbn":"1640781684","title":"Pathfinder - Core Rulebook"}
```

```shell
✗ time curl -sX 'GET' 'http://host:8080/fibonacci/fast/35'  | jq '.'
{
  "result": 9227465,
  "callCount": 0,
  "cached": true,
  "host": "89f3a297fc1d"
}
curl -sX 'GET' 'http://host:8080/fibonacci/fast/35'  0.01s user 0.00s system 65% cpu 0.010 total
jq '.'  0.03s user 0.00s system 99% cpu 0.025 total
```

There are some non-obvious fields on the objects our API returns. We can ask the API what they are[^jqshowoff]:
```shell
✗ curl -s http://localhost:8080/v3/api-docs \
| jq '.components.schemas."FibonacciResult".properties' \
| jq 'to_entries' \
| jq 'reduce .[] as $i ({}; .[$i.key] += ($i.value * {"field": $i.key}))' \
| jq '.["callCount", "host", "cached"]' \
| jq -c '{field, description}'

{"field":"callCount","description":"The number of intermediate, recursive calls made to obtain this result."}
{"field":"host","description":"Which host serviced the request that provided this value?"}
{"field":"cached","description":"Was this value retrieved from the cache?"}
```


### Single JVM
The only examples of Spring declarative caching I found online were those of the most simple case: a single JVM.  This case is pretty easy to reason about, but it seems hard for me to imagine
an example of where it would be useful in the modern world of distributed applications. 

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
        CI-->>C: PUT
        CI-->>B: return
    else cache hit
        C-->>CI: return
        CI-->>B: return
        end
```

### Multiple JVMs
Nowadays, microservices are all the rage. It's VERY common to come across a distributed application that runs in multiple JVMs. This was exactly the concern I had
with using {{page.cacheable}}.

```shell
✗ curl -s http://host:8080/books/clear; for i in {1..6}; do curl -s http://host:8080/books/0130305529 | jq -c
 
{"host":"0e7b8a14bd32","cached":false}
{"host":"7b9a2ba6757a","cached":true}
{"host":"44b88b21d5ad","cached":true}
{"host":"bd96ee2ec401","cached":true}
{"host":"e7d40c130569","cached":true}
{"host":"0e7b8a14bd32","cached":true}
```

#### Multiple JVMs: Default Behavior

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
        CI-->>C: PUT
        CI-->>B: return
    else cache hit
        C-->>CI: return
        CI-->>B: return
    end
```

#### Multiple JVMs: Central Cache

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
CI-->>C: PUT
CI-->>B: return
else cache hit
C-->>CI: return
CI-->>B: return
end
```

```shell
./gradlew bootRun
```

```shell
curl -s http://localhost:8080/books | jq -c '.[]'
```

```json
{"isbn":"0130305529","title":"On Lisp","author":"Paul Graham"}
{"isbn":"0923891579","title":"Introduction to Meta-Mathematics","author":"Stephen Cole Kleene"}
{"isbn":"1640781684","title":"Pathfinder - Core Rulebook","author":"Jason Bulmahn"}
```

```shell
curl -s http://localhost:8080/books/0130305529 | jq -c '.'
```
```json
{"isbn":"0130305529","title":"On Lisp","author":"Paul Graham"}
```

```shell
curl -s http://localhost:8080/books/0130305529/badUpdateTitle/Onto%20Lisp | jq -c '.'
curl -s http://localhost:8080/books/0130305529 | jq -c '.'                           
```

Cache wasn't updated!
```jsonc
{"isbn":"0130305529","title":"On Lisp","author":"Paul Graham"}
```

```java
@Entity
@Data
@NoArgsConstructor
@AllArgsConstructor
class Book {
	@Id @NonNull
	private String isbn;
	@With
	private String title;
	private String author;
}
```

```mermaid!
block-beta
  columns 4
  client["client"]
  space
  block:group1:2
    columns 5
    app(["app"]) space cache[("cache")] space db[("db")] 
    app --> cache
    cache --> db
  end
  client --> app
```

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
    columns 3
  
    block:replica1:3
      app1(["app"]) cache1[("cache")] db1[("db")]
      app1 --> cache1
      cache1 --> db1
    end
    
    block:replica2:3
      app2(["app"]) cache2[("cache")] db2[("db")]
      app2 --> cache2
      cache2 --> db2
    end
    
    block:replica3:3
      app3(["app"]) cache3[("cache")] db3[("db")]
      app3 --> cache3
      cache3 --> db3
    end
    
    block:replica4:3
      app4(["app"]) cache4[("cache")] db4[("db")]
      app4 --> cache4
      cache4 --> db4
    end
    
    block:replica5:3
      app5(["app"]) cache5[("cache")] db5[("db")]
      app5 --> cache5
      cache5 --> db5
    end
    
  end
  
  client --> lb
  lb --> app1
  lb --> app2
  lb --> app3
  lb --> app4
  lb --> app5
```

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
      app1(["app"]) cache1[("cache")]
      app1 --> cache1
    end
    
    block:replica2:2
      app2(["app"]) cache2[("cache")]
      app2 --> cache2
    end
    
    block:replica3:2
      app3(["app"]) cache3[("cache")]
      app3 --> cache3
    end
    
    block:replica4:2
      app4(["app"]) cache4[("cache")]
      app4 --> cache4
    end
    
    block:replica5:2
      app5(["app"]) cache5[("cache")]
      app5 --> cache5
    end
    
  end
  
  client --> lb
  lb --> app1
  lb --> app2
  lb --> app3
  lb --> app4
  lb --> app5
  
  db[("db")]:1
  
  cache1 --> db
  cache2 --> db
  cache3 --> db
  cache4 --> db
  cache5 --> db
```

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
  
  cache[("cache")]:1
  
  db[("db")]:1
  
  cache --> db
```


<!---@formatter:off--->
[^baeldung]: Seriously, I'm not picking on Baeldung. It's one of my favorite web sites. Even though I didn't love its examples in this instance, the article I referenced served as
    one of the starting points for this very blog post.

[^function]: What would be the point of caching a method whose return value was dependent on something _other_ than its input values? Think about it...

[^jqshowoff]: You could just look at the Swagger UI, which shows these descriptions. But I said I was going to use {{page.curl}} and {{page.jq}}. I'm committed.
<!---@formatter:on--->
