{ pkgs }: {
  deps = with pkgs; [
    clang
    just
    zig
  ];
}
