Notes On Word2Vec and NaNoGenMo
===============================

While working on this project several things about using Word2Vec occured to me.  It is useful to say a few works about the technology before hand, though.

About Word2Vec
--------------

Word2Vec performs a delightful transposition from words to vectors.  That is to say, given a corpus to digest, word2vec associates each word in the corpus with a vector (a list of numbers).  Remarkably, these vectors retain both an interesting relationship with their associated words _and_ behave somewhat like real, bona-fide vectors.  The [word2vec page](https://code.google.com/p/word2vec/) contains some interesting examples, but here is one from [my NaNoGenMo project](https://github.com/VincentToups/SwannsWayThroughTheNightLand).

    Word: odette  Position in vocabulary: 127

									  Word       Cosine   distance
------------------------------------------------------------------
								   himself      0.945930
								verdurins'      0.909001
									   him      0.896716
										he      0.893593
								  odette's      0.864390
									 swann      0.862793
									wished      0.850552
								 suspicion      0.850157
							   forcheville      0.845592
									proved      0.840949
								 suspected      0.840874
									  fact      0.839297
								  happened      0.833970
								 affection      0.831837
									letter      0.826493
								  probably      0.826252
								  realised      0.825312

This little table demonstrates one interesting quality of the word2vec vectors: that related vectors point to similar locations in the vector space.  Further surprising results may obtain: 