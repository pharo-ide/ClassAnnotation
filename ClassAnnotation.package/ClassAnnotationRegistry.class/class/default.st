accessing
default
	^default ifNil: [ default := self collectFromSystem ]