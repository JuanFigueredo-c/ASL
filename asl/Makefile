# Comando para compilación silenciosa
build-silent:
	stack build --silent

# Comando para compilación rápida con salida mínima
build-fast:
	stack build --fast --color never

# Comando para compilación estándar
asl:
	stack build

# Comando para la ejecución de un programa
run: 
	stack exec -- asl-exe $(filter-out $@,$(MAKECMDGOALS)) 
