# Markov text generator
##### A Markov chain-based text randomizer written in Haskell


### How do I get this to run?

Compile with `ghc randomizer.hs` and run with `./randomizer <input file> <sentence count>`. If you're on Windows, converting these commands to Windows command prompt commands shouldn't be too hard.

### What does it do?

The program reads the text file you give as its argument and compiles a statistic of how often words follow after other words (aka counts word pairs). It then builds sentences not unlike a Markov chain, using a weighted choice algorithm to choose the following words in the sentence. The first word of every sentence is the only word that is chosen without considering weight. A sentence ends when the generator picks a word that ends with a '.', '!' or a '?'. You control how many sentences it generates with the sentence count parameter.


### That's cool and all, but what does it *actually* do?

In this repository, you'll find a couple of test files you can run the program with to see what it does. The following files are present:

* `hgttg.txt` - a chapter from the Hitchhiker's Guide to the Galaxy
* `trump.txt` - a speech Donald Trump made in September 2017 during his first United Nations address
* `cuni.txt` - a brief excerpt from the history of the Charles University
* `simple.txt` - two simple sentences, there are only a couple of sentences that can actually be generated
* `empty.txt` - in case you want to try to break my program and/or don't know how to make an empty file

*made as part of my non-procedural programming lab at MFF CUNI*
