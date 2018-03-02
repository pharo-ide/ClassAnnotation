redefining
cleanRedefinedGarbage
	"We should remove here all obsolete annotations which not exist in cache anymore.
	It can happen for several reasons: 
		- annotation method was removed.
		- annotated class was removed
		- and various changes related to class hierarchy"	
	redefinedInstances ifNil: [ ^self].
	
	(redefinedInstances select: [ :each | each isObsolete ])
		do: [ :each | redefinedInstances removeKey: each ]