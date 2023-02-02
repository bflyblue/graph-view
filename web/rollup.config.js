import typescript from "@rollup/plugin-typescript";
import { defineConfig } from "rollup";

export default defineConfig({
  input: "src/main.ts",
  output: {
    file: "dist/bundle.js",
    format: "iife",
  },
  plugins: [typescript()],
});
