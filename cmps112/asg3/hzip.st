#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/smalltalk/bin/gst -f
"|___________opt flags + arguments___________|"

opt_D := false.
opt_T := false.
opt_C := false.
opt_U := false. 
flagError := false.
nl := Character nl.

Smalltalk arguments: '-d -t -c -u'
do: [:opt :arg |

   arg = nil ifTrue: [

   (opt asInteger) = 100 ifTrue: [ opt_D := true. ].
   (opt asInteger) = 116 ifTrue: [ opt_T := true. ].
   (opt asInteger) = 99 ifTrue: [ opt_C := true. ].
   (opt asInteger) = 117 ifTrue: [ opt_U := true. ].

   "-t, -c, or -u specified"
   opt_T ifTrue: [ 
       opt_C ifTrue: [ flagError := true. ].
       opt_U ifTrue: [ flagError := true. ].
   ].

   opt_C ifTrue: [ 
       opt_T ifTrue: [ flagError := true. ].
       opt_U ifTrue: [ flagError := true. ].
   ].

   opt_U ifTrue: [ 
       opt_C ifTrue: [ flagError := true. ].
       opt_T ifTrue: [ flagError := true. ].
   ].
   
   flagError ifTrue: [
       status := 1.
       stderr << 'Error: ' << 'Quitting with status ' << status << nl.
       ObjectMemory quit: status.
   ]. ].

]
ifError: [
   'Error: invalid option' displayNl.
   '--foo'
].

(Smalltalk arguments size) > 3 ifTrue: [
       status := 1.
       stderr << 'Error: ' << 'Greater than 3 Arguments'<<nl <<
       'Exit Status: ' << status << nl.
       ObjectMemory quit: status.
   ].
(Smalltalk arguments size) < 2 ifTrue: [
       status := 1.
       stderr << 'Error: ' << 'Fewer than 2 Arguments'<<nl <<
       'Exit Status: ' << status << nl.
       ObjectMemory quit: status.
   ].


"__________________ Out file (if it exists) (if not stdout)___________"
outFile := ''.
|out_file|
(Smalltalk arguments size) = 3 ifTrue: [
   outFile := (Smalltalk arguments at: 3).
   out_file := FileStream open: outFile
                    mode: FileStream write. ]. 

(Smalltalk arguments size) = 2 ifTrue: [ 
    out_file := stdout. ].

inFile := (Smalltalk arguments at: 2).
FileStream extend [
   nl := Character nl.
   catFile [
      |count|
      count := 0.
      [self atEnd not] whileTrue: [
         count := count + 1.
         count asString size to: 5 do: [:skip| stdout << ' '].
         stdout << count << ': ' << self nextLine << nl.
      ]
   ]
]
|in_file|
in_file := FileStream open: inFile
                    mode: FileStream read.

inputReset := [
    in_file close.
    in_file := FileStream open: inFile
                        mode: FileStream read.
].



"|_____________bitstream.st & outbits.st_______________|"

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
      currentByte = 8 ifFalse: [
         myStream nextPutByte: currentByte.
         self clearByte.
      ]
   ]
   writeBit: bit [
      currentByte := currentByte bitAt: bitIndex put: bit.
      bitIndex := bitIndex - 1.
      bitIndex = 0 ifTrue: [self flushByte].
   ]
]


"______________Decomp Helper + inits______________"
sortcol := SortedCollection new.
myCol := OrderedCollection new.
AArray := #().
DArray := #().
|bFile|
bFile := OutBits new: out_file.

convbit := [:index|
    encodStr := encodeTable at: index.
    encodStr do: [:ch |         
        (ch asInteger) = 48 ifTrue: [ AArray := AArray, #(0). ].
        (ch asInteger) = 49 ifTrue: [ AArray := AArray, #(1). ].
        "stdout << AArray size << nl."
    ].
].

decimalBin := '' asString.
decToBin := [:n |
    |num bit quotient count|
    decimalBin := '' asString.
    count := 0.
    num := n.
    [num > 0] whileTrue: [ 
        bit := num\\2.
        quotient := num quo:2.
        temp := decimalBin.
        decimalBin := bit asString, temp.
        num := quotient.
        count := count +1.
    ].
    count to: 7 do: [:x |
        decimalBin := '0', decimalBin.
    ]. 
    decimalBin do: [:ch |         
       (ch asInteger) = 48 ifTrue:[DArray := DArray, #(0).].
       (ch asInteger) = 49 ifTrue:[DArray := DArray, #(1).].
    ].
].

lVal := 0.
huffman := OrderedCollection new.
bin2Dec := [:x |
    |result num|
    lVal := 0.
    num := 128.
    result := 0.
    x do: [:ch | 
        ch asInteger = 49 ifTrue: [
            result := result + num.
        ].
        num := num/2.
    ].
    result = 128 ifTrue:[ result := 256.].
    lVal := result.
].



"________________tree.st_____________________"
Object subclass: Leaf [
   |char count str|
   char [ ^ char ]
   count [ ^ count ]
   str [ ^ str ]
   Leaf class >> new [
      self shouldNotImplement
   ]
   Leaf class >> new: aChar count: aCount[
      |result|
      result := super new.
      result setChar: aChar andCount: aCount.
      ^result
   ]
   isLeaf [
      ^true
   ]
   setChar: aChar andCount: aCount [
      char := aChar.
      count := aCount.
   ]
   setStr: aStr [
      str := aStr.
   ]
   <= other [
      ^ (count < other count)
      | ((count = other count) & (char <= other char))
   ]
   printBase: aStream [
      ^ aStream << self class << '(' << char << ',' << count << 
                                                         ',' << str
   ]
   printOn: aStream [
      (self printBase: aStream) << ')'.
   ]
   inorder: visitor prefix: string [
      visitor value: char value: string.
      self setStr: string.
   ]
   "____________Added Stuff Here___________"
   inorderBits: visitor myCol: col [
      col add: self.
   ]   
   postorder: visitor myCol: sortcol[
      sortcol add: self.
   ]
   simple: n bitArr: AArray [
      |num bit quotient count decimalBin BArray temp|
      decimalBin := '' asString.
      count := 0.
      num := n.
      BArray := AArray.
      [num > 0] whileTrue: [ 
          bit := num\\2.
          quotient := num quo:2.
          temp := decimalBin.
          decimalBin := bit asString, temp.
          num := quotient.
          count := count +1.
      ].
      count to: 7 do: [:x |
           decimalBin := '0', decimalBin.
      ].
      decimalBin do: [:ch |         
           (ch asInteger) = 48 ifTrue: [ 
               BArray := BArray, #(0). ].
           (ch asInteger) = 49 ifTrue: [ 
               BArray := BArray, #(1). ].
       ].
       n = 256 ifTrue: [
          BArray := BArray, #(1). ].
       n = 0 ifTrue: [
          BArray := BArray, #(0). ].
      ^BArray.
   ]
   postorderBits: visitor bitArr: AArray [
      |BArray|
       BArray := AArray, #(0).
       ^BArray := (self simple: (self char) bitArr: BArray).
   ]
]
"_____________End of changes made____________________"

Leaf subclass: Tree [
   |left right|
   left [ ^ left ]
   right [ ^ right ]
   Tree class >> new: aChar count: aCount [
      self shouldNotImplement
   ]
   Tree class >> new: aChar count: aCount left: aLeft right: aRight [
      |result|
      result := super new: aChar count: aCount.
      result setLeft: aLeft andRight: aRight.
      ^ result
   ]
   isLeaf [
      ^false
   ]
   setLeft: aLeft andRight: aRight [
      left := aLeft.
      right := aRight.
   ]
   printOn: aStream [
      (self printBase: aStream) << ',' << left << ',' << right << ')'.
   ]
   inorder: visitor prefix: string [
      left inorder: visitor prefix: string, '0'.
      right inorder: visitor prefix: string, '1'.
   ]
   "__________Added Here____________"
   inorderBits: visitor myCol: col [
      left inorderBits: visitor myCol: col.
      right inorderBits: visitor myCol: col.
   ]    
   postorder: visitor myCol: sortcol[
      left postorder: visitor myCol: sortcol.
      right postorder: visitor myCol: sortcol.
   ]
   postorderBits: visitor bitArr: AArray [
       |temp BArray|
       BArray := left postorderBits: visitor bitArr: AArray.
       BArray := BArray, (right postorderBits: visitor bitArr: AArray).
       ^BArray := BArray, #(1).
   ]    
]

"______________charfreq.st______________________"
Character extend [
   isPrint [
      ^ (Character space <= self) & (self <= $~)
   ]
   visible [
      self isPrint ifTrue: [^  self asString]
                   ifFalse: [^ self asInteger printStringRadix: 8]
   ]
]

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
   at: index put: value[
      ^ theArray at: index + 1 put: value.
   ]
   incr: index [
      (self at: index) isNil ifTrue: [ self at: index put: 0].
      self at: index put: (self at: index) + 1.
   ]
   keysAndValuesDo: aBlock [
      (0 to: (self size) - 1) do: [:index |
         aBlock value: index value: (self at: index).
      ]
   ]
]

"___________Our frequency table___________"

opt_U ifFalse: [
    freqtable := ZeroArray new: 257.

    [in_file atEnd not] whileTrue: [
        |ordChar|
        ordChar := in_file next asInteger. 
        freqtable incr: ordChar.
    ].
    freqtable incr: 256.
    freqtable keysAndValuesDo: [:key :value |
        (value notNil and: [value > 0]) ifTrue: [
            temp := Leaf new: key count: value.
            sortcol add: temp.
        ].
    ].
].


"_____________________Huffman Tree_______________________"

opt_U ifFalse: [

    [(sortcol size) > 1] whileTrue:[
        leftnode := sortcol removeFirst.
        rightnode := sortcol removeFirst.
        str := (leftnode char)+ (rightnode char).
        answer := Tree new: str count: ((leftnode count) + 
        (rightnode count)) 
                         left: leftnode right: rightnode.
        sortcol add: answer.
    ]. 
    tree := sortcol removeFirst.
    tree inorder: [:char :string | ]prefix: ''.
    tree postorder: [:char :string | ] myCol: sortcol. 

    sortcolChar := SortedCollection new.

    sortcol do: [:element | 
        sortcolChar add: element. 
    ]. 
    sortcolChar sortBlock: [:a :b | a char < b char].
].

"_____________Encoding Table______________"
opt_U ifFalse: [
    encodeTable := ZeroArray new: 257.

   sortcol do:[:element |
      encodeTable at: (element char) put: (element str).
   ].
].

opt_T ifTrue:[
   (0 to: encodeTable size - 1)
   do: [:index |
      |char greatert32 lesst127 cnt string|
      greatert32 := index > 32.
      lesst127 := index < 127.
      cnt := freqtable at: index.
      string := encodeTable at: index.
      cnt isInteger ifTrue: [
         greatert32 & lesst127 ifFalse: [
            greatert32 ifFalse: [ stdout << ' '. ].
            stdout << index << ' '.
            1 to: (2 - (cnt size)) do: [:x | stdout << ' '. ].
            99 < cnt ifFalse: [ stdout << ' '. ].
            9 < cnt ifFalse: [ stdout << ' '. ].
            stdout << cnt << '  ' << string << nl.
         ] ifTrue: [
            char := index asCharacter.
            1 to: (1 - (char size)) do: [:x | stdout << ' '. ].
            stdout << char << '    '.
            1 to: (2 - (cnt size)) do: [:x | stdout << ' '. ].
            stdout << cnt << '  ' << string << nl.
         ] .
      ].
   ].
].
"________Comp Helper_____"
   writeAArray := [:outfile|
   |outBytes i|
   i := 0.
   outBytes := OutBits new: outfile.
   AArray do: [:bit| outBytes writeBit: bit. i := i + 1.].
   outBytes writeBit: 0.
   outBytes flushByte.
].

"____________Compression________________"

opt_C ifTrue: [
    inputReset.
    AArray := (tree postorderBits: [] bitArr: AArray).

    in_file close.
    in_file := FileStream open: inFile
                        mode: FileStream read.
    [in_file atEnd not] whileTrue: [
        |index|
        index := in_file next asInteger.
        convbit value: index.
    ].   
    convbit value: 256.
    writeAArray value: out_file.
].




"______________Decompression______________________"
bit_in := #().
addBit := [:charBit|
    charBit = 0 ifTrue:[ bit_in := bit_in, #(0). ].
    charBit = 1 ifTrue:[ bit_in := bit_in, #(1). ].
].
opt_U ifTrue: [
    decomp := true.
    stdinBits := BitStream new: in_file.

    [decomp] whileTrue: [
       curr := stdinBits nextBit.
           curr = 0 ifTrue: [ 
           temp := '' asString.
           bit_in := #().
           1 to:8 do: [:x |
               curr := (stdinBits nextBit).
               addBit value: curr.
               temp := temp, curr asString.
           ].
           temp = '10000000' ifTrue: [ 
               "stdout << '10000000' << nl."
               curr := (stdinBits nextBit).
               addBit value: curr.
               temp := temp, curr asString.
           ].
           temp = '00000000' ifTrue: [ 
               stdout << '0000000' << nl.
               curr := (stdinBits nextBit).
               addBit value: curr.
               temp := temp, curr asString.
           ].
           bin2Dec value: temp.
           tLeaf := Leaf new: lVal count: lVal.
           huffman addFirst: tLeaf.
       ] 
       ifFalse: [ 
           leaf1 := huffman removeFirst.
           (huffman size) = 0 ifTrue:[
               decomp := false.
               huffman addFirst: leaf1.
           ] ifFalse:[
               leaf0 := huffman removeFirst.
               t := Tree new: '' count: 0 left: leaf0 right: leaf1.
               huffman addFirst: t.
           ].
        ].
    ].

    first := huffman removeFirst.
    pntr := first.
 
    (first isLeaf) ifFalse: [ 
        [stdinBits atEnd] whileFalse: [
        curr := stdinBits nextBit.
            curr = 0 ifTrue:[
               pntr := pntr left
               ]ifFalse:[
                pntr := pntr right
               ].
               (pntr isLeaf) ifTrue: [
                   (pntr char) = 256 ifTrue: [  
                       [stdinBits atEnd] whileFalse: [
                           curr := stdinBits nextBit.
                       ].
                   ] ifFalse: [
                       decToBin value: (pntr char).                   
                   ].
                   pntr := first.
               ].           
        ]. 
    ].
    stdout << 'inspecting DArray: ------------' << nl.
    DArray inspect.
    outBytes := OutBits new: out_file.
    DArray do: [:bit| outBytes writeBit: bit].
].
in_file close.
out_file close.     

