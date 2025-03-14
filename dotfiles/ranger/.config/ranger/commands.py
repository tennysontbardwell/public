# This is a sample commands.py.  You can add your own commands here.
#
# Please refer to commands_full.py for all the default commands and a complete
# documentation.  Do NOT add them all here, or you may end up with defunct
# commands when upgrading ranger.

# A simple command for demonstration purposes follows.
# -----------------------------------------------------------------------------

from __future__ import (absolute_import, division, print_function)

# You can import any python module as needed.
import os

# You always need to import ranger.api.commands here to get the Command class:
from ranger.api.commands import Command


# Any class that is a subclass of "Command" will be integrated into ranger as a
# command.  Try typing ":my_edit<ENTER>" in ranger!
class my_edit(Command):
    # The so-called doc-string of the class will be visible in the built-in
    # help that is accessible by typing "?c" inside ranger.
    """:my_edit <filename>

    A sample command for demonstration purposes that opens a file in an editor.
    """

    # The execute method is called when you run this command in ranger.
    def execute(self):
        # self.arg(1) is the first (space-separated) argument to the function.
        # This way you can write ":my_edit somefilename<ENTER>".
        if self.arg(1):
            # self.rest(1) contains self.arg(1) and everything that follows
            target_filename = self.rest(1)
        else:
            # self.fm is a ranger.core.filemanager.FileManager object and gives
            # you access to internals of ranger.
            # self.fm.thisfile is a ranger.container.file.File object and is a
            # reference to the currently selected file.
            target_filename = self.fm.thisfile.path

        # This is a generic function to print text in ranger.
        self.fm.notify("Let's edit the file " + target_filename + "!")

        # Using bad=True in fm.notify allows you to print error messages:
        if not os.path.exists(target_filename):
            self.fm.notify("The given file does not exist!", bad=True)
            return

        # This executes a function from ranger.core.acitons, a module with a
        # variety of subroutines that can help you construct commands.
        # Check out the source, or run "pydoc ranger.core.actions" for a list.
        self.fm.edit_file(target_filename)

    # The tab method is called when you press tab, and should return a list of
    # suggestions that the user will tab through.
    # tabnum is 1 for <TAB> and -1 for <S-TAB> by default
    def tab(self, tabnum):
        # This is a generic tab-completion function that iterates through the
        # content of the current directory.
        return self._tab_directory_content()


import ranger.api
import ranger.api.commands

import subprocess
import tempfile
import json
import os

class utils():
    def probably_not_indexed(self, f, fname):
        # let's check if the file is indexed
        o = subprocess.check_output([
            'git-annex', 'status', f.path, '--json'
        ])
        try:
            status = json.loads(o)['status']
            if status == '?':
                self.fm.notify(
                    "'{}' is not indexed or was renamed. "
                    "You must commit the changes.".format(fname)
                    .encode('utf-8')
                )
                return
        except ValueError:
            self.fm.notify(
                "'{}' is probably not indexed.".format(fname)
                .encode('utf-8')
            )
            return


class ga_tag(ranger.api.commands.Command):
    """:ga_tag <tagname> [tagname...]

    Tags the current file with git-annex metadata.
    """

    def execute(self):
        if not self.arg(1):
            return

        for f in self.fm.thistab.get_selection():
            for tag in self.args[1:]:
                subprocess.check_output([
                    'git-annex', 'metadata', '-t', tag, f.path])

        self.fm.notify('tagged in git-annex!')
        # self.fm.reload_cwd()


class ga_set(ranger.api.commands.Command):
    """:ga_set <key>=<value> [<key>=<value>...]

    Sets the metadata key-value pair in git-annex for the current file.
    "=" can be "+=", "-=" or "?=".
    """

    def execute(self):
        if not self.arg(1):
            return

        for f in self.fm.thistab.get_selection():
            for pair in self.args[1:]:
                if '=' not in pair:
                    continue

                subprocess.check_output([
                    'git-annex', 'metadata', '-s', pair, f.path])

        self.fm.notify('set in git-annex!')
        # self.fm.reload_cwd()


class ga_whereis(ranger.api.commands.Command):
    """:ga_whereis

    Shows in which other git-annex repos the current file is.
    Doesn't show the present repo, since it should be clear from ranger.
    """

    def execute(self):
        f = self.fm.thisfile
        o = subprocess.check_output([
            'git-annex', 'whereis', f.path, '--json'])

        data = json.loads(o)
        places = data['whereis'] + data['untrusted']

        repos = []
        for repo in places:
            if not repo['here']:
                spl = repo['description'].split('[')
                name = spl[-1][:-1] if len(spl) > 1 else repo['description']
                repos.append(name)

        self.fm.notify(' | '.join(repos))


class ga_get(ranger.api.commands.Command, utils):
    """:ga_get

    Fetches current file from a different git-annex remote (or special remote).
    """

    def execute(self):
        for f in self.fm.thistab.get_selection():
            fname = f.basename.decode('utf-8')
            self.fm.notify(
                u"fetching'{}'...".format(fname).encode('utf-8')
            )
            try:
                o = subprocess.check_output([
                    'git-annex', 'get', f.path, '--json'])
            except subprocess.CalledProcessError as e:
                self.fm.notify(e.output, bad=True)
                continue

            try:
                data = json.loads(o)
                if data['success']:
                    self.fm.notify(
                        u"'{}' fetched successfully.".format(fname)
                        .encode('utf-8')
                    )
                else:
                    self.fm.notify(o.encode('utf-8'), bad=True)
            except ValueError:
                if self.probably_not_indexed(f, fname):
                    continue

                self.fm.notify('error: ' + repr(o), bad=True)

        self.fm.reload_cwd()


class ga_drop(ranger.api.commands.Command, utils):
    """:ga_drop

    Drops current file from this git-annex repository here
    (as long as it is present in <numcopies> other repositories).
    """

    def execute(self):
        for f in self.fm.thistab.get_selection():
            fname = f.basename.decode('utf-8')

            self.fm.notify(
                u"dropping '{}'...".format(fname).encode('utf-8')
            )
            try:
                o = subprocess.check_output([
                    'git-annex', 'drop', f.path, '--json'
                ])
            except subprocess.CalledProcessError as e:
                self.fm.notify(e.output, bad=True)
                continue

            try:
                data = json.loads(o)
                if data['success']:
                    self.fm.notify(
                        u"'{}' dropped successfully.".format(fname)
                        .encode('utf-8')
                    )
                else:
                    self.fm.notify(o.encode('utf-8'), bad=True)
            except ValueError:
                if self.probably_not_indexed(f, fname):
                    continue

                self.fm.notify('error: ' + repr(o), bad=True)

        self.fm.reload_cwd()


import ranger.api
import ranger.core.linemode

import subprocess
import json
from pathlib import Path
import time

# from cachetools import TTLCache
class TTLCache:
    def __init__(self, ttl):
        self.ttl = ttl  # Time to live in seconds
        self.cache = {}

    def get(self, key):
        if key in self.cache:
            value, expiry = self.cache[key]
            # Check if the cached value is still valid
            if time.time() < expiry:
                return value
            else:
                # Remove expired item
                del self.cache[key]
        return None  # Return None if the item is not found or expired

    def set(self, key, value):
        expiry = time.time() + self.ttl
        self.cache[key] = (value, expiry)

count = 0
annex_cache = TTLCache(ttl=30)
repos_cache = TTLCache(ttl=30)

@ranger.api.register_linemode
class GitAnnexMetadataLinemode(ranger.core.linemode.LinemodeBase):
    name = 'git-annex-metadata'

    def filetitle(self, file, metadata):
        return file.relative_path

    def check_repo_cache(self, file):
        walk = file.parent
        while walk != Path('/'):
            res = self.repos_cache.get(str(walk))
            if res:
                if res == 'repo-root':
                    return walk
                if res == 'no-repo':
                    return None
            walk = walk.parent
        try:
            repo = Path(subprocess.check_output(['git', 'rev-parse', '--show-toplevel']).decode().strip())
            self.repos_cache.set(str(repo), 'repo-root')
            return repo
        except subprocess.CalledProcessError:
            walk = file.parent
            while walk != Path('/'):
                self.repos_cache.set(str(walk), 'no-repo')
                walk = walk.parent

    def check_annex_cache(self, file):
        # repo = self.check_repo_cache(file)
        # if not repo:
        #     return '(no repo)'
        #     raise NotImplementedError

        res = '(not added)'
        def check_cache():
            nonlocal res
            # rel_path = file.relative_to(repo)
            meta = annex_cache.get(str(file))
            # res = str(rel_path); return True
            if meta:
                res = meta
                return True
            else:
                pass
            return False

        if check_cache():
            return res

        paths = [x for x in file.parent.iterdir() if x.is_file() or x.is_symlink()]
        try:
            with open("/Users/tennyson/Desktop/x.json", 'w') as f:
                f.write('asdf\n')
            o = subprocess.check_output(['git-annex', 'metadata', '--json'] + paths,
                stderr=subprocess.STDOUT)
            for x in [json.loads(x) for x in o.decode().splitlines()]:
                try:
                    with open("/Users/tennyson/Desktop/x.json", 'a') as f:
                        f.write(json.dumps(x) + '\n')
                    info = sorted(x['fields']['tag'])
                    annex_cache.set(str(file.parent / x['file']), " ".join(info))
                    with open("/Users/tennyson/Desktop/x.json", 'a') as f:
                        f.write(' '.join(info) + '\n')
                except KeyError:
                    try:
                        with open("/Users/tennyson/Desktop/x.json", 'a') as f:
                            f.write('n/a\n')
                            f.write(str(file.parent / x['file']) + '\n')
                        annex_cache.set(str(file.parent / x['file']), "n/a")
                    except KeyError:
                        pass
            # with open("/Users/tennyson/Desktop/x.json", 'a') as f:
            #     json.dump(paths, f)
            with open("/Users/tennyson/Desktop/x.json", 'a') as f:
                global count
                count += 1
                json.dump(len(annex_cache.cache), f)
                keys = [str(x) for x in annex_cache.cache.keys()]
                json.dump(keys, f)
                json.dump(count, f)
        except subprocess.CalledProcessError:
            # for p in paths:
            #     self.annex_cache.set(p.relative_to(repo), 'error')
            # return 'error'
            raise NotImplementedError

        if check_cache():
            return res
        raise NotImplementedError


    def infostring(self, file, metadata):
        if not file.is_link:
            raise NotImplementedError
        return self.check_annex_cache(Path(file.path))

@ranger.api.register_linemode
class GitAnnexWhereisLinemode(ranger.core.linemode.LinemodeBase):
    name = 'git-annex-whereis'

    def __init__(self):
        self._repositories = None

    @property
    def repositories(self):
        if self._repositories:
            return self._repositories

        try:
            o = subprocess.check_output([
                'git-annex', 'info', '--json'
            ])
        except subprocess.CalledProcessError:
            raise NotImplementedError

        info = json.loads(o)

        self._repositories = {}
        for repo in info['trusted repositories']:
            self._repositories[repo['uuid']] = repo
        for repo in info['semitrusted repositories']:
            self._repositories[repo['uuid']] = repo
        for repo in info['untrusted repositories']:
            self._repositories[repo['uuid']] = repo

        for uuid, repo in self._repositories.items():
            spl = repo['description'].split('[')
            name = spl[-1][:-1] if len(spl) > 1 else repo['description']
            self._repositories[uuid] = name

        return self._repositories

    def filetitle(self, file, metadata):
        return file.relative_path

    def infostring(self, file, metadata):
        if not file.is_link:
            raise NotImplementedError

        try:
            o = subprocess.check_output([
                'git-annex', 'whereis',
                file.path, '--json'],
                stderr=subprocess.STDOUT)
        except subprocess.CalledProcessError:
            raise NotImplementedError

        data = json.loads(o)
        places = data['whereis'] + data['untrusted']
        repos = (self.repositories[r['uuid']] for r in places if not r['here'])

        if len(places) > 2:
            repos = [repo[:5] for repo in repos]
        else:
            repos = list(repos)

        return '[' + '|'.join(repos) + ']'
