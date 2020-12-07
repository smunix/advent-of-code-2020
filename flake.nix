{
  outputs = { self, nixpkgs }: {
    packages.x86_64-linux = with nixpkgs.legacyPackages.x86_64-linux.pkgs; {
      day1 = haskellPackages.callCabal2nix "day1" ./day1 {
        inherit (self.packages.x86_64-linux) utils;
      };
      utils = haskellPackages.callCabal2nix "utils" ./utils {};
      d1 = haskellPackages.callPackage ./d1/d1.nix {
        version = ''${self.rev or self.lastModifiedDate}'';
      };
    };
    defaultPackage = builtins.mapAttrs (_: pkgs: pkgs.utils) self.packages;
  };
}
