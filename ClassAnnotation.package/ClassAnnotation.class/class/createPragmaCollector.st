private
createPragmaCollector

	| collector |
	collector := PragmaCollector filter: [ :pragma | pragma keyword == #classAnnotation].
	collector reset.
	collector whenChangedSend: #resetRegistry to: self.	
	^collector