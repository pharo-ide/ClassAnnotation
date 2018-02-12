operations
debug
	"here we just reevaluate messages which could fail"
	| ann |
	ann := self freshInstance.
	ann isForbidden.
	^ann