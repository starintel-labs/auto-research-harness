# Implementation prompt

Implement the Auto Research Common Lisp runtime described by this `spec/` directory.

Start by reading `00-README.md`, then inspect the existing website repo, current `lost-rob0t/starintel-auto-research` contracts/scripts named in `02-existing-contracts.org`, and the current Prolog-RLM machine interface.

Do **not** load every spec file into the model at once. Work slice-by-slice:

1. architecture + existing contracts
2. CL runtime + project DSL
3. Prolog-RLM direct harness + semantic tools
4. ARADR execution/events
5. Forgejo request/idempotency layer
6. website API/auth/SSE
7. validators/tests/end-to-end slice

Preserve existing Org/approval/run/candidate/ledger contracts. Forgejo/API/cron/agents are callers; CL owns workflow; Prolog-RLM is the semantic backend. Do not implement symbolic mode.

Make the smallest coherent vertical slice first, run real tests/validators, and never claim work passed unless observed.
