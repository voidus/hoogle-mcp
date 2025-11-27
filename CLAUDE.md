# Development Notes for Claude

## Build System

This project uses Nix and direnv for dependency management, NOT plain cabal.

### Important Commands

- **NEVER** run `cabal update` directly
- Use `direnv exec . cabal build` instead of `cabal build` when there are dependency issues
- Dependencies are provided via Nix and loaded through direnv
- If you add dependencies in cabal files or change nix files, you may need to use `direnv exec .` to reload the environment

## Testing Philosophy

When writing tests for functionality, **always encode them as unit tests** for future reference and regression testing. One-off manual tests are useful for initial verification, but should be converted to automated tests if the functionality is meant to be maintained.

## Project Structure

- MCP server for Hoogle (Haskell documentation search)
- Main entry point: `app/Main.hs`
- Library code: `src/`
- Tests: `test/`

## MCP Server Details

The server provides a `hoogle_search` tool that:
- Takes a query string parameter
- Calls `hoogle search -- <query>` on the command line
- Returns the search results to the MCP client
