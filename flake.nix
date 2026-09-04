{
  description = "Auto Research Common Lisp runtime (cl-prolog-rlm harness, ARADR, Forgejo request plane, website API)";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    prolog-rlm = {
      url = "github:lost-rob0t/prolog-rlm";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
      prolog-rlm,
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        lib = pkgs.lib;

        prologRlm = prolog-rlm.packages.${system}.prolog-rlm;
        prologRlmSwipl = "${prologRlm}/bin/prolog-rlm-swipl";

        clDeps = with pkgs.sbcl.pkgs; [
          alexandria
          serapeum
          str
          cl-ppcre
          jzon
          babel
          hunchentoot
          dexador
          quri
          uuid
          local-time
          split-sequence
          ironclad
          bordeaux-threads
          fiveam
        ];

        sbcl = pkgs.sbcl.withPackages (ps: clDeps);

clSystems = [
          "cl-prolog-rlm"
          "starintel-aradr"
          "starintel-research-core"
          "starintel-auto-research"
          "starintel-auto-research-web"
        ];

        autoResearch = pkgs.stdenvNoCC.mkDerivation {
          pname = "auto-research";
          version = "0.1.0";
          src = lib.cleanSource ./.;

          strictDeps = true;
          nativeBuildInputs = [
            sbcl
            pkgs.swi-prolog
          ];

          dontConfigure = true;

          buildPhase = ''
            runHook preBuild
            export HOME="$TMPDIR/home"
            mkdir -p "$HOME"
            export CL_SOURCE_REGISTRY="$PWD//"
            sbcl --non-interactive --script "$PWD/scripts/load-all.lisp"
            runHook postBuild
          '';

          doCheck = true;
          checkPhase = ''
            runHook preCheck
            export HOME="$TMPDIR/check-home"
            mkdir -p "$HOME"
            export CL_SOURCE_REGISTRY="$PWD//"
            export PROLOG_RLM_SWIPL="${prologRlmSwipl}"
            sbcl --non-interactive --script "$PWD/tests/run-tests.lisp"
            runHook postCheck
          '';

          installPhase = ''
            runHook preInstall
            source_root="$out/share/common-lisp/source/auto-research"
            mkdir -p "$source_root" "$out/bin"
            cp -R "$src"/. "$source_root/"

            cat > "$out/bin/auto-research" <<EOF
            #!${pkgs.runtimeShell}
            set -euo pipefail
            source_root="$out/share/common-lisp/source/auto-research"
            export PROLOG_RLM_SWIPL="''${PROLOG_RLM_SWIPL:-${prologRlmSwipl}}"
            exec ${sbcl}/bin/sbcl \
              --script "\$source_root/scripts/main.lisp" \
              "\$@"
            EOF

            chmod +x "$out/bin/auto-research"
            runHook postInstall
          '';

          meta = {
            description = "Auto Research Common Lisp runtime harnessing Prolog-RLM in direct mode";
            license = lib.licenses.agpl3Only;
            mainProgram = "auto-research";
            platforms = lib.platforms.unix;
          };
        };
      in
      {
        packages = {
          default = autoResearch;
          inherit autoResearch;
        };

        apps = {
          default = {
            type = "app";
            program = "${autoResearch}/bin/auto-research";
          };
        };

        checks.default = autoResearch;

        devShells.default = pkgs.mkShell {
          packages = [
            sbcl
            prologRlm
            pkgs.swi-prolog
            pkgs.git
            pkgs.python3
          ];

          shellHook = ''
            export CL_SOURCE_REGISTRY="$PWD//"
            export PROLOG_RLM_SWIPL="${prologRlmSwipl}"
            echo "auto-research dev shell: $(sbcl --version)"
            echo "Prolog-RLM swipl: $PROLOG_RLM_SWIPL"
          '';
        };

        formatter = pkgs.nixfmt-rfc-style;
      }
    );
}
