if exists("g:loaded_tasksearch") || &cp
  finish
endif
let g:loaded_tasksearch = 1

let s:filename = resolve(expand('<sfile>:p'))
let s:folder = fnamemodify(s:filename, ':h')
let s:python_filename = s:folder . '/task_search.py'

function! PythonFile()
  return s:python_filename
endfunction

function! GoToNextTask()
  " Get the current byte offset
  let byte_offset=line2byte(line('.'))+col('.')-2
  " Get the contents of the current buffer
  " TODO: this is hacky and may work incorrectly with other line endings. If we notice issues, pick another approach
  let buf=join(getline(1, '$'), "\n")
  " Find the location of the next task
  let new_offset=system('python ' . PythonFile() . ' next ' . byte_offset, buf)
  " Position the cursor at the next task
  execute 'normal! :goto ' . (new_offset + 1) . "\<cr>"
endfunction

function! GoToPreviousTask()
  " Get the current byte offset
  let byte_offset=line2byte(line('.'))+col('.')-2
  " Get the contents of the current buffer
  " TODO: this is hacky and may work incorrectly with other line endings. If we notice issues, pick another approach
  let buf=join(getline(1, '$'), "\n")
  " Find the location of the next task
  let new_offset=system("python " . PythonFile() . " previous " . byte_offset, buf)
  " Position the cursor at the next task
  execute "normal! :goto " . (new_offset + 1) . "\<cr>"
endfunction

nmap <leader>n :call GoToNextTask()<CR>
nmap <leader>N :call GoToPreviousTask()<CR>

