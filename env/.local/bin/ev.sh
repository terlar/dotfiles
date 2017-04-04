#!/bin/bash
[ "${TRACE}" ] && set -x
set -eu
set -o pipefail

EVPATH="${EVPATH:-$HOME/.config/env}"

print_env() {
	rel_varpath="${1}"
	abs_varpath="${EVPATH}/${rel_varpath}"

	if [ -f "${abs_varpath}" ]
	then
		varname="${rel_varpath##*/}"
		if [ -x "${abs_varpath}" ]
		then
			echo "${varname}"="$(${abs_varpath})"
		else
			echo "${varname}"="$(cat "${abs_varpath}")"
		fi
	elif [ -d "${abs_varpath}" ]
	then
		for i in "${abs_varpath}"/*
		do
			print_env "${i/${EVPATH}\//}"
		done
	else
		>&2 echo "ev: ${rel_varpath} is not a file or directory in EVPATH"
		exit 1
	fi
}

main() {
	if [ ! -d "${EVPATH}" ]
	then
		>&2 echo "ev: EVPATH '${EVPATH}' is not a directory"
		exit 1
	fi
	print_env "${1?DIR or FILE not provided}"
}

main "$@"
