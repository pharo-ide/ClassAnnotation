private
createPragmaCollector

	| collector |
	collector := PragmaCollector filter: [ :pragma | pragma keyword == #classMetaAnnotation].
	collector reset.
	collector whenChangedSend: #resetRegistry to: self.	
	^collector