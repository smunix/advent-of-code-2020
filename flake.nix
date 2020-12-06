{
  outputs = { self, nixpkgs }: {
    packages.x86_64-linux = with nixpkgs.legacyPackages.x86_64-linux.pkgs; {
      utils = haskellPackages.callCabal2nix "utils" ./utils {};
    };
    defaultPackage = builtins.mapAttrs (_: pkgs: pkgs.utils) self.packages;
  };
}
