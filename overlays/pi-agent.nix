final: prev: {
  # The pi-coding-agent is already in nixpkgs and handles the complex
  # monorepo build process for badlogic/pi-mono.
  # We alias it here to 'pi-agent' as requested.
  pi-agent = prev.pi-coding-agent;
}
