accessing
declaredInstancesFor: aClass 
	^(self registry at: self ifAbsent: [ ^#() ])
		select: [ :each | each annotates: aClass ]