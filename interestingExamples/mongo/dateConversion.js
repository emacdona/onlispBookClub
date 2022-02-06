
conn = Mongo(`mongodb://root:${mongopw}@localhost:27017/admin`)
db = conn.getDB("reddit")

db.subreddits.updateMany(
    {},
    [
        {"$set" : {"created": {"$toDate": {"$multiply": ["$created_utc", 1000]}}}}
    ]
)
