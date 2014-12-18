load 'graphics/plot'
load '/home/toups/j64-803-user/projects/spectral_clustering_notes/utils.ijs'
load 'format/printf'

readfile =: 1!:1
writefile =: 1!:2	
w1 =: (readfile <'./resources/tnl-words.txt')
w2 =: (readfile <'./resources/swanns-way-words.txt')

addSpace =: ]@:(,&' ')
spaceCut =: (> ;. _2)@:addSpace

w1 =: spaceCut w1
w2 =: spaceCut w2

allW =: (w1,w2)
uniqueW =: ~. allW

counts =: allW #/. allW

uniqueWSorted =: (/: counts) { uniqueW
countsSourted =: (/: counts) { counts

relativeClassify =: (<.@:([ * (] % (>./)@:])))

class_twenty =: 20 relativeClassify countsSourted
class_twenty_indexes =: ~. class_twenty
class_twenty_samples =: class_twenty (<@:(20&{.))/. uniqueWSorted 

to =: monad define 
  'a b' =. y
  (": a), ' - ', (": b)
)

relativeClassifyTable =: dyad define

 bins =. x
 words =. y
 counts =. words #/. words
 uniqueSorted =. (/: counts) { (~. words)
 countsSorted =. (/: counts) { counts
 classes =. bins relativeClassify countsSorted
 groups =. classes (<@:(40&{.))/. uniqueSorted 
 mx =. >./ countsSorted
 uclasses =. ~. classes
 scaled_uclasses =. <. ((mx % bins) * uclasses)
 scaled_tops =. 1 (|.!.mx) scaled_uclasses
 labels =. (to"1) (|: (scaled_uclasses ,: scaled_tops))
 smoutput scaled_uclasses
 (<"1 labels) ,: groups
)

writeWordCounts =: dyad define
  fout =. x
  words =. y
  counts =. words #/. words		
  uniqueSorted =. (/: counts) { (~. words)
  countsSorted =. (/: counts) { counts
  output =. ('%s %d'&sprintf) (|: (<"1 uniqueSorted) ,: (<"0 countsSorted))
  output fwrites fout
)

NB. './resources/all-word-counts.txt' writeWordCounts allW
