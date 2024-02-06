help([[
Build environment for fit2obs on S4
]])

prepend_path("MODULEPATH", "/data/prod/jedi/spack-stack/spack-stack-1.6.0/envs/gsi-addon-env/install/modulefiles/Core")

local stack_intel_ver=os.getenv("stack_intel_ver") or "2021.5.0"
local stack_impi_ver=os.getenv("stack_impi_ver") or "2021.5.0"

load(pathJoin("stack-intel", stack_intel_ver))
load(pathJoin("stack-impi", stack_impi_ver))

load("fit2obs_common")

whatis("Description: fit2obs environment on S4 with Intel Compilers")
