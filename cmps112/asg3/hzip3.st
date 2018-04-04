#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/smalltalk/bin/gst -f

execname := '$RCSfile: cmdline.st,v $' =~ '.*: (.*),v.*' at: 1.
nl := Character nl.
queue := SortedCollection new.

|decodingtree ExitStatus|


"add helper methods for FileStream class"
FileStream extend [
   nl := Character nl.
   catFile [
      |count textline|
      count := 0.
      [self atEnd not] whileTrue: [
         count := count + 1.
         textline :=self nextLine.
         count asString size to: 5 do: [:skip| stdout << ' '].
         
         stdout << count << ': ' << textline << nl.
      ]
   ]
]

"add helper methods for Character class"
Character extend [
   isPrint [
      ^ (Character space < self) & (self <= $~)
   ]
   digits[
         ^ (self asInteger) > 15
         ]
   visible [
        "stdout << (self asInteger)."
      self isPrint ifTrue: [^' ', self asString, ' ']             
                   ifFalse: [ self digits
                      ifTrue:[((self asInteger) = 256)
                              ifTrue:[ ^ 'EOF']ifFalse:[^ 'x',
                              ((self asInteger printStringRadix: 16)
                               copyFrom: 4 )]
                             ]
                      ifFalse:[
                             
                             ^ 'x0',
                              ((self asInteger printStringRadix: 16)
                               copyFrom: 4)]
                     ]           
   ]
]

"an zeroArray class to store data"
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

   size [
      ^ theArray size.
   ]

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

freqtable := ZeroArray new: 257.
encodetable := ZeroArray new: 257.
freqtable at:256 put:1.

"a leaf data structure to hold character and its
frequency"
Object subclass: Leaf [
   |char count isleaf|
   
   char [ ^ char ]
   count [ ^ count ]
   isleaf [^ isleaf]
   
   Leaf class >> new [
      self shouldNotImplement
   ]

   Leaf class >> new: aChar count: aCount [
      |result|
      result := super new.
      result setChar: aChar andCount: aCount.      
      ^result
   ]

   setChar: aChar andCount: aCount [
      char := aChar.
      count := aCount.
      isleaf:=true.
   ]

   <= other [
      ^ (count < other count)
      | ((count = other count) & (char <= other char))
   ]

   printBase: aStream [
      ^ aStream << self class << '(' << char << ',' << count
   ]

   printOn: aStream [
      (self printBase: aStream) << ')'.
   ]

   inorder: visitor prefix: string [
      visitor value: char value: string.
   ]
   
   postorder: visitor [
      visitor  value:char value: isleaf.
   ]
]

"a tree data structure to hold leaves"
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
      isleaf:=false.
   ]

   left  [ ^ left]
   right [ ^ right]

   printOn: aStream [
      (self printBase: aStream) << ',' << left << ',' << right << ')'.
   ]

   inorder: visitor prefix: string [
      left inorder: visitor prefix: string, '0'.
      right inorder: visitor prefix: string, '1'.
   ]
   
   postorder: visitor[      
      left postorder: visitor .
      right postorder: visitor .  
      visitor value: char value:isleaf.
   ]
]

"a class for writing bits to output file"
Object subclass: OutBits [
   |bitIndex currentByte myStream|
   OutBits class >> new [
      self shouldNotImplement.
   ]
   OutBits class >> new: fileStream [
      |result|
      result := super new.
      result init: fileStream.
      ^result
   ]
   clearByte [
      bitIndex := 8.
      currentByte := 0.
   ]
   init: fileStream [
      myStream := fileStream.
      self clearByte.
   ]
   flushByte [
         myStream nextPutByte: currentByte.
         self clearByte.

   ]
   writeBit: bit [
      currentByte := currentByte bitAt: bitIndex put: bit.
      bitIndex := bitIndex - 1.
      bitIndex = 0 ifTrue: [
        self flushByte].
   ]
]

"a class for reading bits from input file"
Object subclass: BitStream [
   |bitIndex byte myStream|
   BitStream class >> new: fileStream [
      |result|
      result := super new.
      result init: fileStream.
      ^result
   ]
   init: fileStream [
      myStream := fileStream.
      bitIndex := 1.
   ]
   nextBit [
      bitIndex = 1 ifTrue: [
         byte := myStream next.
         bitIndex := 9.
      ].
      bitIndex := bitIndex - 1.
      ^byte value bitAt: bitIndex
   ]
   atEnd [
      ^bitIndex = 1 and: [myStream atEnd]
   ]
]

"main function"

"var to hold pipline for input stream"
|infile infile2 outfile dcpfile outfilename argcount|

dFlag := false.
cFlag := false.
tFlag := false.
uFlag := false.

argcount:=0.


"parse the options and filename"Smalltalk arguments: '-d -t -c -u'
do: [:opt :arg |
   
   (opt = $d) ifTrue: [
   dFlag := true.
   "stdout <<'-d option is set'<<nl."
    ].
    
   (opt = $t) ifTrue: [
   tFlag := true.
   "stdout <<'-t option is set'<<nl."
    ].

  (opt = $c) ifTrue: [
   cFlag := true.
   "stdout <<'-c option is set'<<nl."
   ].

  (opt = $u) ifTrue: [
    uFlag := true.
   "stdout <<'-u option is set'<<nl."
   ].
  
  (arg ~= nil) ifTrue: [
    
     argcount = 0 
        ifTrue:[

          dFlag ifTrue:[
          stdout <<'input filename: ' << arg <<nl.].
             
          infile := FileStream open: arg
                    mode: FileStream read.
          infile2 := FileStream open: arg
                    mode: FileStream read.
          
         ]
        ifFalse:[
           dFlag ifTrue:[
           stdout <<'output filename: ' << arg <<nl.].
            outfile := FileStream open: arg
                       mode: FileStream write.].
  
     argcount := argcount + 1.
   ].
].


uFlag ifFalse:[


"build frequency table"
[infile atEnd not] whileTrue: [
   |ordChar|
   ordChar := infile next asInteger.
   freqtable incr: ordChar.
].

"stdout <<  'freqtable at:256  '<<(freqtable at: 256)<< nl.
"
freqtable keysAndValuesDo: [:key :count |
   (count notNil and: [count > 0]) ifTrue: [
      queue add: (Leaf new: key count: count).
      "stdout <<  key asCharacter visible << '     ' << count
             << nl."
   ]
].

"stdout <<'output from sortedCollection'<< nl.
queue do: [:item| stdout << item << nl.]."

"stdout << nl << '----bulid tree-----' << nl."
[queue size >1 ] whileTrue: [
   |leftchild rightchild|
   
   leftchild := queue removeFirst.
   rightchild := queue removeFirst.
   newtree := ( Tree new: (leftchild char)
                      count: (leftchild count) +  (rightchild count)
                      left: leftchild right: rightchild).
   queue add: newtree.
].
decodingtree:=queue removeFirst.

"stdout << nl << 'build encode table' << nl."
decodingtree inorder: [:char :string |
   encodetable at: char put: string.
   "stdout <<  char asCharacter visible<<'    '<< (freqtable at: char) 
               <<'        ' << string << nl."
] prefix: ''.

"convert decoding tree into binary"
binarytree:=''.
decodingtree postorder: [:char :isleaf |

   isleaf  ifFalse:[binarytree:=binarytree,'1'.]
                ifTrue:[
                   bin:=''.
                   char = 256
                        ifTrue:[bin:='000000001']
                        ifFalse:[
                           char=0 ifTrue:[bin:='000000000']
                                  ifFalse:[
                                     8 to:1 by: -1 do:[:i|
                                     bin:=bin,((char bitAt:i)
                                                       printString).
                                   ]
                        ].               
                 ].
                 binarytree:=(binarytree,'0',bin).
              ].
].

"decoding tree and table is printed to stdout"
tFlag ifTrue:[
     stdout <<nl<< '------------decoding tree--------------' << nl.
     stdout << decodingtree << nl<<nl.
     stdout <<'binary decoding tree: '<<nl<<binarytree << nl.

     stdout <<nl<< '------------encoding table-------------' << nl.
     encodetable keysAndValuesDo: [:key :count |
     (count notNil and: [count > 0]) ifTrue: [
        stdout <<  key asCharacter visible << '     '
               << (freqtable at: key) <<'    '<<count
               << nl.
     ].
    ].
].


"perform compression"
cFlag  ifTrue:[ 

outfile~=nil ifTrue:[
   
    dFlag ifTrue: [
    stdout <<nl<< 'cFlag = '<<cFlag 
          <<'  output file = true'<< nl.].

   "output binary decoding tree"   
   outBytes := OutBits new: outfile.    
   1 to: (binarytree size) by: 1 do: [:index |
         bit:= ((binarytree at: index) asInteger) - 48.
         outBytes writeBit: bit.
         ].

   "at the end add 1 bit for checking 
      whether the tree is completed"  
   outBytes writeBit: 1.

   "output encoded data" 
   [infile2 atEnd not] whileTrue: [
      |ordChar2|
       ordChar2 := infile2 next asInteger.
       code:= (encodetable at: ordChar2). 
       1 to: (code size) by: 1 do: [:index |
         bit:= ((code at: index) asInteger) - 48.
         outBytes writeBit: bit.
          ].
       ].

   eof:= (encodetable at: 256).
   1 to: (eof size) by: 1 do: [:index |
         bit:= ((eof at: index) asInteger) - 48.
         outBytes writeBit: bit.].     
   outBytes flushByte.
]
ifFalse:[
   dFlag ifTrue:[
   stdout <<nl<< 'cFlag = '<<cFlag
              <<' without output file'<< nl.].
    
   stdout <<nl<< '----------encoded input in binary----------' << nl.
    "add at the end 1 bit for checking 
      whether the tree is completed"
   stdout <<binarytree<<1.      
    
     [infile2 atEnd not] whileTrue: [
           |ordChar2|
           ordChar2 := infile2 next asInteger.
           code:= (encodetable at: ordChar2).
           stdout <<code. 
        ].
      eof:= (encodetable at: 256).
      stdout<<eof.
   ].    
 ].
].


"code for decompression and -u is set"
(uFlag) ifTrue:[
   |iseof outBytes stack|
   iseof:=false.

   dFlag ifTrue: [
   stdout<<'doing decompression'<<nl.].

   outBytes := OutBits new: outfile.  
   stack := OrderedCollection new.
   fileBits := BitStream new: infile.
   treebuilt:=false.
   reconstructedTree:=''.
   node:=''.   
   
   "read each bit from the input file"
   [(fileBits atEnd not) & (iseof not)] whileTrue: [

     treenode:=fileBits nextBit.
     leafchar:=0.

     "if the decoding tree is still constructing"
     treebuilt ifFalse:[

        "convert the input code into a leaf"        
        treenode=0 ifTrue:[

           8 to:1 by:-1 do:[:i|
              bit:=fileBits nextBit.
              
            leafchar:=leafchar bitAt:i put:bit.                        
           ].

           leafchar=0 ifTrue:[
             specialbit:=fileBits nextBit.
             specialbit=1 ifTrue:[
              leafchar:=leafchar bitAt:9 put:1.
             ].
           ].
         
          stack add: (Leaf new: leafchar count: '').

        ].

        "build a tree from two leaf" 
        treenode=1 ifTrue:[
                 
           stack size>1 ifTrue:[
           rightchild:=stack removeLast.
           leftchild:=stack removeLast.
           
           newtree := (Tree new: -1
                      count: ''
                      left: leftchild right: rightchild).
             stack add: newtree.
           ] ifFalse:[
             treebuilt:=true.
             
             reconstructedTree:=stack removeLast.
             node:=reconstructedTree.
     
            "outfile=nil ifTrue:[
            stdout<<nl<<
                '---------decoded output-----------'<<nl<<nl.]."
 
           ].
        ].
     ]
     ifTrue:["<-- when the tree is reconstructed
                                   decode the compressed data"
        
       node isleaf ifTrue:[
                ((node char)=256) ifTrue:[
                    iseof:=true.
                    ] ifFalse:[
                     stdout nextPut: (Character value: (node char)).
                  node:=reconstructedTree.].
               ]ifFalse:[
        treenode=0 ifTrue:[
                
                node:=node left.

                node isleaf ifTrue:[
                iseof not ifTrue:[  
                    ((node char)=256) ifTrue:[
                    iseof:=true.


                    ] ifFalse:[

                     outfile=nil ifTrue:[

                     stdout nextPut: (Character value: (node char)).
                     "stdout<<(node char) asCharacter."]
                     ifFalse:[
                        8 to: 1 by: -1 do: [:index |
                           bit:= (node char) bitAt: index.
                           outBytes writeBit: bit.
                        ].
                     ].
                     node:=reconstructedTree.

                    ].
                ].
               ]
           ] ifFalse:[
               
               node:=node right.

               node isleaf ifTrue:[
                iseof not ifTrue:[
                    ((node char)=256) ifTrue:[
                    iseof:=true.

                    ] ifFalse:[

                     outfile=nil ifTrue:[

                     stdout nextPut: (Character value: (node char)).
                     "stdout<<(node char) asCharacter."]
                     ifFalse:[
                        8 to: 1 by: -1 do: [:index |
                           bit:= (node char) bitAt: index.
                           outBytes writeBit: bit.
                        ].
                     ].
                    node:=reconstructedTree.

                    ].
                ].
               ]
           ].
       ].
     ].
   ].
].  


infile close.
outfile~=nil ifTrue:[
outfile close.
].
ExitStatus:=0.
dFlag ifTrue: [
stdout<<nl<<'Exit Status: '<<ExitStatus<<nl.
stdout<<'Finished '<<nl.
].
