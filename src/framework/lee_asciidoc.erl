%%--------------------------------------------------------------------
%% Copyright (c) 2023 k32. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%--------------------------------------------------------------------

%% @doc This module enriches schema with the documentation obtained
%% from the external source (asciidoc files)
-module(lee_asciidoc).

%% behavior callbacks:
-export([enrich_model/2]).

-export_type([options/0]).

-include_lib("xmerl/include/xmerl.hrl").

%%================================================================================
%% Type declarations
%%================================================================================

-type options() ::
        #{ asciidoctor_exec => file:filename_all()
         , root_directory   => file:filename_all()
         , readme_file      => file:filename_all()
         , doc_file_pattern => file:filename_all()
         , tmp_directory    => file:filename_all()
         }.

%%================================================================================
%% behavior callbacks
%%================================================================================

-spec enrich_model(options(), lee:module()) -> lee:module().
enrich_model(Options, Model) ->
    {ok, DefaultRootDir} = file:get_cwd(),
    Default = #{ asciidoctor_exec => os:find_executable("asciidoctor")
               , root_directory   => DefaultRootDir
               , readme_file      => "README.adoc"
               , doc_file         => "doc/src/schema.adoc"
               , tmp_directory    => "_build/lee_doc/docbook"
               },
    #{ asciidoctor_exec := AsciidoctorExec
     , root_directory   := RootDir
     , readme_file      := ReadmeFile
     , doc_file         := DocFile
     , tmp_directory    := TmpDirectory
     } = maps:merge(Default, Options),
    ok = filelib:ensure_dir(filename:join(TmpDirectory, "dummy")),
    %% Transform asciidoc files to docbook:
    Readme = preprocess_asciidoc(AsciidoctorExec, TmpDirectory, filename:join(RootDir, ReadmeFile)),
    Rest = preprocess_asciidoc(AsciidoctorExec, TmpDirectory, filename:join(RootDir, DocFile)),
    %% Enrich model:
    lee_model:map_vals(
      fun(Key, {MTs, Attrs}) ->
              case lists:member(doc_root, MTs) of
                  true ->
                      XML = Readme,
                      OnelinerXpath = "/article/info/title/text()",
                      DocXpath = "/article/*[not(self::info) and not(self::title)]";
                  false ->
                      XML = Rest,
                      Id = lee_doc:format_key(Key),
                      OnelinerXpath = "/article/section[@xml:id=\"" ++ Id ++ "\"]/title/text()",
                      DocXpath = "/article/section[@xml:id=\"" ++ Id ++"\"]/*[not(self::info) and not(self::title)]"
              end,
              {MTs, maybe_add_doc(XML, DocXpath, maybe_add_oneliner(XML, OnelinerXpath, Attrs))}
      end,
      Model).

%%================================================================================
%% Internal functions
%%================================================================================

preprocess_asciidoc(AsciidoctorExec, TmpDirectory, Filename) ->
    OutFile = filename:join(TmpDirectory, filename:basename(Filename) ++ ".xml"),
    Args = ["-o", OutFile, "-b", "docbook5", Filename],
    Port = erlang:open_port({spawn_executable, AsciidoctorExec}, [nouse_stdio, {args, Args}, exit_status]),
    receive
        {Port, {exit_status, ExitStatus}} ->
            0 = ExitStatus, %% Assert
            {XmlDoc, []} = xmerl_scan:file(OutFile, [{document, false}]),
            XmlDoc
    end.

maybe_add_oneliner(XML, XPath, Attrs) ->
    case xmerl_xpath:string(XPath, XML) of
        [#xmlText{value = Oneliner}] ->
            Attrs#{oneliner => Oneliner};
        [] ->
            Attrs
    end.

maybe_add_doc(XML, XPath, Attrs) ->
    case xmerl_xpath:string(XPath, XML) of
        [] ->
            Attrs;
        Doc ->
            Attrs#{doc => Doc}
    end.
