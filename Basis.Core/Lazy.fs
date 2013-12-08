namespace Basis.Core

module Lazy =
  [<CompiledName "Value">]
  let value (x: Lazy<_>) = x.Value

  [<CompiledName "Force">]
  let force () (x: Lazy<_>) = x.Force()