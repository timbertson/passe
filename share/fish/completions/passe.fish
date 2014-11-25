# for use in aliases; you can write a "p.fish" completion file with just:
# source /path/to/passe.fish $argv[1]
# and it'll add completions for the `p` binary, rather than `passe`
if test (count $argv) -gt 1
	set cmd $argv[2]
else
	set cmd $argv[1]
end

set -l cmd (basename $cmd .fish)
function __fish_passe_complete
	begin;
		passe --list 2>/dev/null
	end
end

complete -e -c $cmd
complete -c $cmd -s(passe --help | grep -o -E -- ' -[a-z]' | cut -c 2-)
complete -c $cmd -l(passe --help | grep -o -E -- ' --[a-z]+' | cut -c 4-)
complete -c $cmd --no-files --arguments '(__fish_passe_complete)'
