{
  description = "status-notifier-item";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    git-ignore-nix = {
      url = "github:hercules-ci/gitignore.nix/master";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };
  outputs = { self, flake-utils, nixpkgs, git-ignore-nix }:
  let
    overlay = final: prev: {
      haskellPackages = prev.haskellPackages.override (old: {
        overrides = prev.lib.composeExtensions (old.overrides or (_: _: {}))
        (hself: hsuper: {
          status-notifier-item =
            hself.callCabal2nix "status-notifier-item"
            (git-ignore-nix.lib.gitignoreSource ./.)
            { };
        });
      });
    };
  in flake-utils.lib.eachDefaultSystem (system:
  let pkgs = import nixpkgs { inherit system; overlays = [ overlay ]; config.allowBroken = true; };
  in
  rec {
    devShell = pkgs.haskellPackages.shellFor {
      packages = p: [ p.status-notifier-item ];
      buildInputs = [
        pkgs.zlib.dev
      ];
    };
    defaultPackage = pkgs.haskellPackages.status-notifier-item;
  }) // {
    inherit overlay;
    overlays.default = overlay;
  };
}
