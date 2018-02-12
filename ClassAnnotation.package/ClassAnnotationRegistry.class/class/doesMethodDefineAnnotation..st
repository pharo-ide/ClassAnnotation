system changes
doesMethodDefineAnnotation: aMethod
	"The second condition allows to mark methods 
	which are sent inside annotation definition methods 
	which makes them annotation dependency.
	For example CmdShortcutCommandActivation annotation has method #removaFor:.
	It provides reusable cmd+x shorcut. And all removal related commands are depends on it.
	When user will modify this method the annotation registry will be updated 
	because of <classAnnotationDependency> pragma"
	
	^(aMethod hasPragmaNamed: self annotationPragmaName)
		or: [ aMethod hasPragmaNamed: self annotationDependencyPragmaName ]