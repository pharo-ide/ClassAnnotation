accessing
registeredInstancesAnnotating: aClass 
	"it will return all my instances which were registered and which annotate given class"
	
	^(self registry at: self ifAbsent: [ ^#() ])
		select: [ :each | each annotates: aClass ]