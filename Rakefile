require 'rake/clean'

outputdir = 'obj'
bindir = 'bin'
o_name = 'goodTuring'
['.'].each do |dir|
  CLEAN.include("#{outputdir}/**/*.hi", "#{outputdir}/**/*.o")
end
CLOBBER.include("#{bindir}/#{o_name}")

# -keep-hc-files 
task :default do
  # -Odph: enables special optimizations to help fusion.  (Includes -O2.)
  # http://donsbot.wordpress.com/2010/02/26/fusion-makes-functional-programming-fun/
  # Using LLVM:  -fllvm
  # http://donsbot.wordpress.com/2010/02/21/smoking-fast-haskell-code-using-ghcs-new-llvm-codegen/
  `ghc -Odph --make Statistics/SGT/Main.hs -outputdir #{outputdir} -o #{bindir}/#{o_name}`
end
