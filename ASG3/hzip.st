#!/usr/bin/gst -f
"#!/afs/cats.ucsc.edu/courses/cse112-wm/usr/smalltalk/bin/gst -f"
"$Id: charfreq.st,v 1.3 2020-01-31 17:09:03-08 - - $"


nl := Character nl.

Character extend [
   isPrint [
      ^ (Character space <= self) & (self <= $~)
   ]
   visible [
      self isPrint ifTrue: [^ '$', self asString]
                   ifFalse: [^ self asInteger printStringRadix: 8]
   ]
]

"Class: BitStream"
Object subclass: BitStream [
   |bitIndex byte myStream writeIndex writeByte|

   "Class Constructor"
   BitStream class >> new: fileStream [
      |result|
      result := super new.
      result init: fileStream.
      ^result
   ]

   "Function: init"
   "Description: initializes BitStream fields"
   "Parameters: fileStream - the fileStream to be used by the bitStream"
   init: fileStream [
      myStream := fileStream.
      bitIndex := 1.
      writeIndex := 9.
      writeByte := 0.
   ]

   "Function: nextBit"
   "Description: Gets the next bit in the current byte"
   "             or the first bit in the next byte"
   nextBit [
      bitIndex = 1 ifTrue: [
         byte := myStream next.
         bitIndex := 9.
      ].
      bitIndex := bitIndex - 1.
      ^byte value bitAt: bitIndex
   ]

   "Function: atEnd"
   "Description: Returns true if at end of current byte"
   "             and at the end of the bitStream"
   atEnd [
      ^bitIndex = 1 and: [myStream atEnd]
   ]

   "Function: currByte"
   "Description: Returns the current byte"
   currByte [
      ^byte
   ]
]

Object subclass: OutBits [
   |bitIndex currentByte myStream|

   "default Constructor"
   OutBits class >> new [
      self shouldNotImplement.
   ]

   "Constructor"
   OutBits class >> new: fileStream [
      |result|
      result := super new.
      result init: fileStream.
      ^result
   ]

   "Function: clearByte"
   "Description: sets Byte to 0000, sets bitIndex to back of byte"
   clearByte [
      bitIndex := 8.
      currentByte := 0.
   ]

   "Function: init"
   "Description: Sets myStream = fileStream, clears byte"
   "Parameters: fileStream"
   init: fileStream [
      myStream := fileStream.
      self clearByte.
   ]

   "Function: flushByte"
   "Description: "
   flushByte [
      bitIndex = 8 ifFalse: [
         myStream nextPutByte: currentByte.
         self clearByte.
      ]
   ]

   "Function: writeBit"
   "Description: replace the bit at the currentByte's"
   "             bitIndex with <bit>"
   "Parameters: bit - the new bit value"
   writeBit: bit [
      currentByte := currentByte bitAt: bitIndex put: bit.
      bitIndex := bitIndex - 1.
      bitIndex = 0 ifTrue: [self flushByte].
   ]
]

"Class: Leaf"
"Description: A node that contains data"
Object subclass: Leaf [
   |char count|
   char [ ^ char ]
   count [ ^ count ]

  "Default Constructor"
   Leaf class >> new [
      self shouldNotImplement
   ]

   "Constructor (character, count)"
   Leaf class >> new: aChar count: aCount [
      |result|
      result := super new.
      result setChar: aChar andCount: aCount.
      ^result
   ]

   "Function: setChar <aChar> andCount <aCount>"
   "Description: sets leaf char to <aChar> and count to <aCount>"
   "Parameters: aChar - Character value of the leaf"
   "            aCount - Number of instances of that char"
   setChar: aChar andCount: aCount [
      char := aChar.
      count := aCount.
   ]

   "Class comparator: <="
   "Description: Compares two leaves and returns bool"
   "             true if this <= other, or false otherwise"
   <= other [
      ^ (count < other count) | ((count = other count) & (char <= other count) & (char <= other char))
   ]

   "Function: printBase"
   "Description: prints out char and count"
   printBase: aStream [
      ^ aStream << self class << '(' << char << ',' << count
   ]

   "Function: aStream"
   "Description: print's aStream base"
   printOn: aStream [
      (self printBase: aStream) << ')'.
   ]

   "Class function: depthFirst <visitor> prefix <string>"
   depthFirst: visitor prefix: string [
      visitor value: char value: string.
   ]

   "Function: postOrder"
   "Description: Writes bytes based on "
   "Parameters: outBytes - the var to write bytes to"
   postOrder: outBytes[
      outBytes writeBit: 0.
      (1 to: 8) do: [:index |
	       outBytes writeBit: ((self char asInteger) bitAt: index).
	    ].
      char asInteger = 0 ifTrue: [
          outBytes writeBit: 0.
      ].
      char asInteger = 256 ifTrue: [
          outBytes writeBit: 1.
      ]
    ]
]

"tree to insert leaf nodes into"
Leaf subclass: Tree [
   |left right|

   Tree class >> new: aChar count: aCount [
      self shouldNotImplement
   ]

   Tree class >> new: aChar count: aCount left: aLeft right: aRight [
      |result|
      result := super new: aChar count: aCount.
      result setLeft: aLeft andRight: aRight.
      ^ result
   ]

   setLeft: aLeft andRight: aRight [
      left := aLeft.
      right := aRight.
   ]

   printOn: aStream [
      (self printBase: aStream) << ',' << left << ',' << right << ')'.
   ]

   left [^ left]
   right [^ right]

   depthFirst: visitor prefix: string [
	"self inspect."
      left depthFirst: visitor prefix: string, '0'.
      right depthFirst: visitor prefix: string, '1'.
   ]

   postOrder: outBytes [
      "Postorder (Left,Right, Root)"
      left postOrder: outBytes.
      right postOrder: outBytes.
      "for each tree print out a 1 byte"
      outBytes writeBit: 1.
   ]
]

"taken from charfreq.st in example-files"
Object subclass: ZeroArray [
   |theArray|

   ZeroArray class >> new [
      self shouldNotImplement.
   ]

   ZeroArray class >> new: size [
      |result|
      result := super new.
      result init: size.
      ^ result
   ]

   init: size [
      theArray := Array new: size.
   ]

   size [ ^ theArray size.  ]

   at: index [
      ^ theArray at: index + 1.
   ]

   at: index put: value [
      ^ theArray at: index + 1 put: value.
   ]

   incr: index [
      (self at: index) isNil
         ifTrue: [ self at: index put: 0].
      self at: index put: (self at: index) + 1.
   ]

   keysAndValuesDo: aBlock [
      (0 to: (self size) - 1) do: [:index |
         aBlock value: index value: (self at: index).
      ]
   ]

]

"@@@@@@@@@@MAIN@@@@@@@@@@@"

dFlag := false. "debug"
tFlag := false. "decoding tree cout"
cFlag := false. "infile compression"
uFlag := false. "infile uncompression"
inputfile := ''.
outputfile := stdout.

argsize := Smalltalk arguments size.
(argsize < 1) ifTrue: [
    stdout << 'Not enough arguments' << nl.
    ObjectMemory quit: 0
].

(1 to: argsize) do: [:index |
	 arg := Smalltalk arguments at: index.
	 (arg size <= 3) ifTrue: [ "Must be one of the 4 options"
	  	(arg includes: $d) ifTrue: [dFlag := true.].
		  (arg includes: $t) ifTrue: [tFlag := true.].
		  (arg includes: $c) ifTrue: [cFlag := true.].
		  (arg includes: $u) ifTrue: [uFlag := true.].
	 ] ifFalse: [
		  (inputfile = '') ifTrue: [inputfile := arg.]
				 ifFalse: [outputfile := arg.]
	 ]
].

input := FileStream open: inputfile mode: FileStream read.
output := stdout.
(outputfile ~= stdout) ifTrue: [
   output := FileStream open: outputfile mode: FileStream write.
].

freqtable := ZeroArray new: 257.

"3a"
"counts number of times each character appears in input file"
[input atEnd not] whileTrue: [
   |ordChar|
   ordChar := input next asInteger.
   freqtable incr: ordChar.
].

"EOF"
freqtable incr: 256.

priorityQueue := SortedCollection new.
priorityQueue := SortedCollection sortBlock: [:a :c |
        (a count < c count)
        | ((a count = c count) & (a char < c char)) ].

"3b"
freqtable keysAndValuesDo: [:key :value |
   |aLeaf aChar aCount|
   (value notNil and: [value > 0]) ifTrue: [
      aCount := value.
      aChar := key.
      aLeaf := Leaf new: aChar count: aCount.
      priorityQueue add: aLeaf.
   ]
].

"priorityQueue inspect."

"3c"

[priorityQueue size > 1] whileTrue: [
   |leaf1 leaf2 tree newChar newCount|
	 leaf1 := priorityQueue removeFirst.
	 leaf2 := priorityQueue removeFirst.
      newChar := leaf1 char.
      newCount := leaf1 count + leaf2 count.
      tree := Tree new: newChar count: newCount
   left: leaf1 right: leaf2.
	 priorityQueue add: tree.
].
"priorityQueue inspect."

TREE := priorityQueue first.
"TREE inspect."

"3d"
encodetable := ZeroArray new: 257.
TREE depthFirst: [:char :string |
   encodetable at: char asInteger put: string.
] prefix: ''.

"3e"

tFlag ifTrue:[

   dFlag ifTrue: [ "DEBUG"
      stdout <<  'Printing decoding tree to output' << nl.
   ].

   encodetable keysAndValuesDo: [:key :value |
 	    over32 := key > 32.
	    under127 := key < 127.
	    (value notNil) ifTrue: [
     	  	aCount := freqtable at: key.
      		(over32 & under127) ifTrue: [
	            stdout << ' ' << key asCharacter  <<
			        ' ' << aCount << ' ' << value <<  nl.
     	   ] ifFalse: [
	          stdout << key asCharacter visible  <<
        	     '    ' << aCount << ' '  << value <<  nl.
     		 ]
      ]
   ].
].

"3f/g"
outBytes := OutBits new: output.
cFlag ifTrue:[

   dFlag ifTrue: [ "DEBUG"
      stdout << 'Writing Encoding Table' << nl.
   ].

   TREE postOrder: outBytes.
	 "end of tree"
	 outBytes writeBit: 1.
	 "padding not needed"
	 input reset.
	 [input atEnd not] whileTrue: [
      |ordChar str|
  		ordChar := input next asInteger.
		  str := encodetable at: ordChar.
		  str do: [:index |
			(index asString = '0') ifTrue: [
				 outBytes writeBit: 0.
	    ] ifFalse: [outBytes writeBit: 1.].
   ]
	 ].
	 strtmp := encodetable at: 256.
	 strtmp do: [:index |
      (index asString = '0') ifTrue: [
         outBytes writeBit: 0.
      ] ifFalse: [outBytes writeBit: 1.].
   ].
	 outBytes flushByte.
].

input close.
output close.

"-u flag passed, uncompressing input file"
uFlag ifTrue: [
	 |finished decodedTree stack outputStream root EOF|
	 output := stdout.
   "if outputfile is not stdout, set output to outputfile"
	 (outputfile ~= stdout) ifTrue: [
      output := FileStream open: outputfile mode: FileStream read.
   ].

   outputStream := BitStream new: output.
	 stack := OrderedCollection new.
	 decodedTree := nil.
	 finished := false.
	 [(outputStream atEnd not) & (finished = false)]  whileTrue: [
	    bit := outputStream nextBit.
		  (bit = 0) ifTrue:  [
			hex := 0.
			(1 to: 8) do: [:base |
			   currbit := outputStream nextBit.
				 (currbit = 1) ifTrue: [
				    n := 2 raisedTo: (base-1).
					  hex := hex + n.
				 ].
			].
			(hex = 0) ifTrue: [
			   currbit := outputStream nextBit.
				 (currbit = 1) ifTrue: [
				    hex := hex + 256.
				 ].
			].
			stack addFirst: (Leaf new: hex asCharacter count: 1).
		  ] ifFalse: [
			   (stack size > 0) ifTrue: [
			      (stack size = 1) ifTrue: [
			   	     decodedTree := stack removeFirst.
				       finished := true.
			      ] ifFalse: [
				       subtree1 := stack removeFirst.
				       subtree2 := stack removeFirst.
				       stack addFirst: (Tree new: (subtree2 char)
					     count: 1 left: subtree2 right: subtree1).
			      ].
         ].
      ].
   ].
	 EOF := false.
	 [(outputStream atEnd not) & (EOF = false)]  whileTrue: [
	    root := decodedTree.
		  [(root isMemberOf: Leaf) | outputStream atEnd] whileFalse: [
			   bit:= outputStream nextBit.
			   (bit = 0) ifTrue: [root := root left.
				 ] ifFalse: [ root := root right.
				 ].
      ].
      (root isMemberOf: Leaf) ifTrue: [stdout << root char.
	       (root char asInteger = 256) ifTrue: [EOF := true.]
	    ].
   ].
]
