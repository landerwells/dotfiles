{
  inputs,
  lib,
  pkgs,
  ...
}: let
  # Keep Ollama on nixpkgs master while nixos-unstable catches up.
  # The Vulkan build is used because it works with the RTX 4070 without
  # pulling the full CUDA toolchain into this configuration.
  ollamaPkgs = import inputs.nixpkgs-latest {
    system = pkgs.stdenv.hostPlatform.system;
    config.allowUnfree = true;
  };
  ollamaOverlay = final: prev: {
    ollama-vulkan = ollamaPkgs.ollama-vulkan;
  };
in {
  nixpkgs.overlays = [ollamaOverlay];

  services.ollama = {
    enable = true;
    package = pkgs.ollama-vulkan;

    # Smallest Linux-compatible Qwen 3.5 27B tag currently published by Ollama.
    # This still needs the RTX 4070 to be detected; CPU-only mode cannot fit it
    # in 16GB RAM.
    loadModels = ["qwen3.5:27b-q4_K_M"];

    environmentVariables = {
      OLLAMA_FLASH_ATTENTION = "1";
      OLLAMA_KV_CACHE_TYPE = "q4_0";
      OLLAMA_CONTEXT_LENGTH = "2048";
      OLLAMA_MAX_LOADED_MODELS = "1";
      OLLAMA_NUM_PARALLEL = "1";
      LD_LIBRARY_PATH = "/run/opengl-driver/lib";
      # Leave headroom for the compositor/display on a 12GB RTX 4070.
      OLLAMA_GPU_OVERHEAD = "1073741824";
    };
  };

  systemd.services.ollama.serviceConfig = {
    # NVIDIA's Vulkan shader/JIT path needs executable memory mappings.
    MemoryDenyWriteExecute = lib.mkForce false;
    Restart = "on-failure";
  };
}
