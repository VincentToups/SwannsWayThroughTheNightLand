# Swann's Way Through The Night Land

This project uses word2vec (ancillarily) and Emacs Lisp to generate a
new novel by substituting similar sentences from one novel for the
sentences of another.  The structure of the first is preserved, but
the content becomes like the second.

See the file output/swanns-way-through-the-nightland.txt for an
example.

You can download the file vectors.txt from here:

http://procyonic.org/vectors.txt

You'll have to change the line:

    (load-word-vectors "/scratch/vectors.txt")

To point to wherever you download this file, if you want to run the
project.

[Word2Vec](https://code.google.com/p/word2vec/) was used to produce this file.  

This technique is a textual transposition of my previous generative
art-work: [Meat is Mulder](http://procyonic.org/meatIsMulder/meatIsMulder.html).

# About the Author

[Procyonic](http://procyonic.org).

