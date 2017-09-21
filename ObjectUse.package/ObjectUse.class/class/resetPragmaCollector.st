private
resetPragmaCollector
	<script>
	DeclarationPragmaCollector ifNil: [ ^self ].
	DeclarationPragmaCollector release.
	DeclarationPragmaCollector := nil