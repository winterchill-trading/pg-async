# AGENTS.md: .github/

`workflows/ci.yml`, the only workflow of this repository.

- The job installs `libpq-dev` and `postgresql` with apt. The `ox-setup` composite action from
  `winterchill-trading/infra` restores the OxCaml switch from cache; the `obj-magic` job calls
  a reusable workflow from the same repository.
- The build line must match the commands in the root AGENTS.md. Changing a step here changes
  what agents are told to run, so update both.
- The concurrency block cancels superseded runs of the same ref; keep it.
- To check a run, use `gh run list --limit 1`. Do not poll in a loop.
