import { defineConfig } from "rollup";
import typescript from "@rollup/plugin-typescript";
import html from "@rollup/plugin-html";

export default defineConfig({
  input: ["src/main.ts", "index.html"],
  output: {
    dir: "dist",
    format: "iife",
  },
  plugins: [typescript(), html()],
});
