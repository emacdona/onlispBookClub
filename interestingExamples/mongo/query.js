
conn = Mongo(`mongodb://root:${mongopw}@localhost:27017/admin`)
db = conn.getDB("reddit")

var d = new Date();
d.setFullYear(d.getFullYear() - 15)

cursor = db
    .subreddits
    .find({
        "over18": false,
        "lang": "en",
        "allow_images": false,
        "subscribers": {"$lt": 1000},
        "created": {"$lt": d}
    })
    .map(subreddit => {
        return {
            "subscribers": subreddit.subscribers,
            "name": subreddit.display_name,
            "url": `http://www.reddit.com${subreddit.url}`,
            "created_epoch": subreddit.created_utc,
            "created": subreddit.created
        }
    })

cursor.forEach(
    record =>
    printjson(record)
)

console.log(`total records in query: ${cursor.size()}`)
console.log(`total records in db: ${db.subreddits.find().size()}`)
