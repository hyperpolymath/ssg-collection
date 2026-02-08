# SPDX-License-Identifier: AGPL-3.0-or-later
# SPDX-FileCopyrightText: 2025 hyperpolymath
# Language Policy Enforcement Rules for robot-repo-cleaner
#
# This file defines rules for detecting and fixing language policy violations
# across all hyperpolymath repositories.

module LanguagePolicyRules

using Base.Filesystem

# Banned extensions and their replacements
const BANNED_EXTENSIONS = Dict(
    ".ts" => "ReScript (.res)",
    ".tsx" => "ReScript (.res)",
    ".go" => "Rust (.rs)",
)

# Allowed TypeScript exceptions (type declarations)
const ALLOWED_TS_PATTERNS = [
    r"\.d\.ts$"
]

# Banned files (Node.js/npm artifacts)
const BANNED_FILES = Set([
    "package-lock.json",
    "yarn.lock",
    "pnpm-lock.yaml",
    "bun.lockb",
])

# Python is only allowed in these paths
const ALLOWED_PYTHON_PATHS = [
    "salt/",
    "saltstack/",
    "_salt/",
]

struct Violation
    repo::String
    file::String
    reason::String
    suggestion::String
end

"""
    scan_repo(repo_path::String) -> Vector{Violation}

Scan a repository for language policy violations.
"""
function scan_repo(repo_path::String)::Vector{Violation}
    violations = Violation[]
    repo_name = basename(repo_path)

    for (root, dirs, files) in walkdir(repo_path)
        # Skip hidden directories and common non-source dirs
        filter!(d -> !startswith(d, ".") && d ∉ ["node_modules", "_site", "target", "_build"], dirs)

        for file in files
            filepath = joinpath(root, file)
            relpath = replace(filepath, repo_path => "")
            relpath = lstrip(relpath, '/')

            # Check banned extensions
            for (ext, replacement) in BANNED_EXTENSIONS
                if endswith(file, ext)
                    # Check for allowed patterns (e.g., .d.ts)
                    if any(r -> occursin(r, file), ALLOWED_TS_PATTERNS)
                        continue
                    end

                    push!(violations, Violation(
                        repo_name,
                        relpath,
                        "Banned extension $ext",
                        "Convert to $replacement"
                    ))
                end
            end

            # Check banned files
            if file in BANNED_FILES
                push!(violations, Violation(
                    repo_name,
                    relpath,
                    "Banned file (Node.js/npm artifact)",
                    "Remove and use Deno (deno.json) instead"
                ))
            end

            # Check Python files
            if endswith(file, ".py")
                is_allowed = any(p -> startswith(relpath, p), ALLOWED_PYTHON_PATHS)
                if !is_allowed
                    push!(violations, Violation(
                        repo_name,
                        relpath,
                        "Python outside SaltStack",
                        "Convert to ReScript or Rust"
                    ))
                end
            end
        end
    end

    violations
end

"""
    fix_violation(v::Violation, repo_path::String; dry_run::Bool=true) -> Bool

Attempt to fix a violation. Returns true if fixed successfully.
For now, only deletes banned files. TypeScript→ReScript requires manual conversion.
"""
function fix_violation(v::Violation, repo_path::String; dry_run::Bool=true)::Bool
    filepath = joinpath(repo_path, v.file)

    if basename(v.file) in BANNED_FILES
        if dry_run
            println("  [DRY-RUN] Would delete: $(v.file)")
        else
            rm(filepath)
            println("  [FIXED] Deleted: $(v.file)")
        end
        return true
    end

    # TypeScript/Go/Python require manual conversion
    println("  [MANUAL] $(v.file) - $(v.suggestion)")
    false
end

"""
    scan_all_repos(repos_dir::String) -> Dict{String, Vector{Violation}}

Scan all repositories in a directory.
"""
function scan_all_repos(repos_dir::String)
    results = Dict{String, Vector{Violation}}()

    for entry in readdir(repos_dir)
        repo_path = joinpath(repos_dir, entry)
        if isdir(repo_path) && isdir(joinpath(repo_path, ".git"))
            violations = scan_repo(repo_path)
            if !isempty(violations)
                results[entry] = violations
            end
        end
    end

    results
end

"""
    print_report(results::Dict{String, Vector{Violation}})

Print a formatted report of all violations.
"""
function print_report(results::Dict{String, Vector{Violation}})
    total = sum(length(v) for v in values(results); init=0)

    println("="^60)
    println("LANGUAGE POLICY VIOLATION REPORT")
    println("="^60)
    println()

    if isempty(results)
        println("No violations found!")
        return
    end

    for (repo, violations) in sort(collect(results))
        println("Repository: $repo ($(length(violations)) violations)")
        println("-"^40)
        for v in violations
            println("  File: $(v.file)")
            println("  Issue: $(v.reason)")
            println("  Fix: $(v.suggestion)")
            println()
        end
    end

    println("="^60)
    println("Total: $total violations in $(length(results)) repositories")
    println("="^60)
end

# Main entry point when run as script
if abspath(PROGRAM_FILE) == @__FILE__
    if length(ARGS) < 1
        println("Usage: julia language-policy-rules.jl <repos_dir> [--fix]")
        println()
        println("Scans repositories for language policy violations.")
        println()
        println("Options:")
        println("  --fix     Attempt to fix violations (deletes banned files)")
        println("  --dry-run Show what would be fixed without making changes (default)")
        exit(1)
    end

    repos_dir = ARGS[1]
    do_fix = "--fix" in ARGS
    dry_run = !do_fix

    println("[POLICY] Scanning repositories in: $repos_dir")
    results = scan_all_repos(repos_dir)
    print_report(results)

    if do_fix && !isempty(results)
        println()
        println("[POLICY] Attempting to fix violations...")
        for (repo, violations) in results
            repo_path = joinpath(repos_dir, repo)
            for v in violations
                fix_violation(v, repo_path; dry_run=dry_run)
            end
        end
    end
end

export Violation, scan_repo, scan_all_repos, fix_violation, print_report

end # module
