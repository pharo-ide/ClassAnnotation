operations
debug
	"here we just reevaluate messages which could fail"
	| ann |
	ann := annotatedClass perform: declarationSelector.
	ann isForbidden.
	^ann