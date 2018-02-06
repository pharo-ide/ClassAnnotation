pragma collecting
annotationDependencyPragmaName
	"This pragma is used to mark methods which are sent inside annotation definition methods.
	They are annotation dependencies.
	For example CmdShortcutCommandActivation annotation has method #removaFor:.
	It provides reusable cmd+x shorcut. And all removal related commands are depends on it.
	When user will modify this method the annotation registry will be updated"
	
	^#classAnnotationDependency