# Noodle - The doodle

![Screenshot](https://raw.github.com/kmerz/noodle/master/noodle.png)

Noodle is a doodle clone written in haskell with:
* [scotty](https://github.com/scotty-web/scotty)
* [persistent](https://github.com/yesodweb/persistent)
* [sqlite3](https://www.sqlite.org/)

Even though it is a project to explore the possibilties of haskell in web
development, it is designed as a inhouse doodle solution to fit the needs of
privacy.

To build the project you need a updated version of cabal and following
commands:

```
cabal configure
cabal install
cabal build
```

After that place `./dist/build/noodle/noodle` and `noodle.css` beside each other
into the desired directory and start it with: `./noodle` or a shell script
or something.

If you liked this project please have a look at my paste bin clone in rails:
[graveio](https://github.com/kmerz/graveio)
