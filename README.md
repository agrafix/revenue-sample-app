# Revenue Sample App

Small example demonstrating different features of [Spock](http://www.spock.li).

## Building

```bash
git clone https://github.com/agrafix/revenue-sample-app
stack build --pedantic
```

## Running

```bash
stack exec revenue-app
# on different terminal
sqlite3 -init sample-data.sql database.sql
[Ctrl+C]
```

## Try it out

* Visit http://localhost:3000 in our browser
* Login with `admin` and `admin1`.
* Extract the value of the cookie `spockcookie`
* Try the rest api:

```bash
# should fail
curl -v --cookie "spockcookie=[COOKIE_VALUE_HERE]" -d '{"name": "Foo", "price": -1, "revenue": 5}' -H 'Content-Type: application/json' -H 'Accept: application/json' -X POST http://localhost:3000/api/item/create

# should succeed
curl -v --cookie "spockcookie=[COOKIE_VALUE_HERE]" -d '{"name": "FooBar", "price": 5.00, "revenue": 0.05}' -H 'Content-Type: application/json' -H 'Accept: application/json' -X POST http://localhost:3000/api/item/create
```
