add_library(clarityfrontend_stuff clarity_language.cpp typecast.cpp clarity_convert.cpp clarity_grammar.cpp pattern_check.cpp)
target_include_directories(clarityfrontend_stuff
    PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}
    PRIVATE ${CLANG_INCLUDE_DIRS}
    PRIVATE ${Boost_INCLUDE_DIRS}
)

target_link_libraries(clarityfrontend_stuff gotoprograms nlohmann_json::nlohmann_json clangcfrontend_stuff)

add_library(clarityfrontend INTERFACE)
target_link_libraries(clarityfrontend INTERFACE clarityfrontend_stuff)
