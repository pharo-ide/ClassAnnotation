accessing
freshInstance
	"Returnes new annotation instance by evaluating declaration method.
	So new fresh instance will be return which can be different then current one"
	
	^annotatedClass perform: declarationSelector